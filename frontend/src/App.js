import CodeContainer from "./CodeContainer";
import UploadCode from "./UploadCode";
import DebuggerPanel from "./DebuggerPanel";
import PointInfoPanel from "./PointInfoPanel";
import { useEffect, useState } from "react";
import './App.css';
import SolutionInfoPanel from "./SolutionInfoPanel";
import RejectionInfoPanel from "./RejectionInfoPanel";
import TimeInfoPanel from "./TimeInfoPanel";

import io from 'socket.io-client';
const socket = io();

function App() {
  const [code, setCode] = useState("");
  const [debug, setDebug] = useState(undefined);
  const [codeHighlight, setCodeHighlight] = useState({"info": "encounters", "style": "color"});
  const [pointDebug, setPointDebug] = useState(undefined);
  const [fileName, setFileName] = useState("");

  useEffect(() => {
    socket.on('connect', () => {
      console.log("Connected to server");
    });

    socket.on('disconnect', () => {
      console.log("Disconnected from server");
    });

    socket.on('exit', (code, signal) => {
      console.log("Racket process exited with code", code, "and signal", signal);
    });

    return () => {
      socket.off('connect');
      socket.off('disconnect');
      socket.off('exit');
    };
  }, []);

  const sendKill = () => {
    socket.emit('kill', (data) => {
      console.log(data);
    });
  };

  const sendCode = (file, fileName) => {
    socket.emit('code', file, fileName, (data) => {
      console.log(data);
      setCode((new TextDecoder("utf-8")).decode(data.file));
      setDebug(undefined);
      setFileName(data.fileName);
    });
  };

  const sendQuery = (query) => {
    socket.emit('query', query, (data) => {
      console.log(data);
      setDebug(data.data)
    });
  }

  return (
    <div>
      <div>
        <UploadCode sendCode={sendCode} fileName={fileName}/>
        <DebuggerPanel sendQuery={sendQuery} sendKill={sendKill} fileName={fileName}/>
      </div>
      <div>
        <div onChange={(event) => setCodeHighlight({...codeHighlight, "info": event.target.value})}>
          <label>Encounters</label>
          <input type={"radio"} name={"PP-Select"} defaultChecked={true} value={"encounters"}/>
          <label>Failures</label>
          <input type={"radio"} name={"PP-Select"} value={"failures"}/>
          <label>Failure Ratio</label>
          <input type={"radio"} name={"PP-Select"} value={"failRatio"}/>
          <label>Successes</label>
          <input type={"radio"} name={"PP-Select"} value={"successes"}/>
          <label>Success Ratio</label>
          <input type={"radio"} name={"PP-Select"} value={"successRatio"}/>
        </div>
        {/*<select defaultValue={"color"} onChange={(event) => setCodeHighlight({...codeHighlight, "style": event.target.value})}>*/}
        {/*  <option value={"none"}>None</option>*/}
        {/*  <option value={"color"}>Color</option>*/}
        {/*  <option value={"bars"}>Bars</option>*/}
        {/*</select>*/}
        <div style={{display: "flex", flexDirection:"row", height: "calc(100vh - 6em)", width: "100vw"}}>
          <CodeContainer code={code} debug={debug} codeHighlight={codeHighlight} setPointDebug={setPointDebug}/>
          <div style={{width: "-webkit-fill-available"}}>
            <TimeInfoPanel debug={debug}/>
            <PointInfoPanel pointDebug={pointDebug} code={code}/>
            <SolutionInfoPanel debug={debug} code={code}/>
            <RejectionInfoPanel debug={debug} code={code}/>
          </div>
        </div>
      </div>
    </div>
  );
}

export default App;
