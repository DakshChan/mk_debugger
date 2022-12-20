import CodeContainer from "./CodeContainer";
import UploadCode from "./UploadCode";
import PointInfoPanel from "./PointInfoPanel";
import { useEffect, useState } from "react";
import './App.css';
import SolutionInfoPanel from "./SolutionInfoPanel";
import RejectionInfoPanel from "./RejectionInfoPanel";
import TimeInfoPanel from "./TimeInfoPanel";
import RunResume from "./RunResume";
import QueryForm from "./QueryForm";

import io from 'socket.io-client';
import QueryPanel from "./QueryPanel";
const socket = io();

function App() {
  const [code, setCode] = useState("");
  const [debug, setDebug] = useState(undefined);
  const [codeHighlight, setCodeHighlight] = useState({"info": "encounters", "style": "color"});
  const [pointDebug, setPointDebug] = useState(undefined);
  const [fileName, setFileName] = useState("");
  const [running, setRunning] = useState(false);
  const [timeInfo, setTimeInfo] = useState(null);
  const [queries, setQueries] = useState({});

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
      setRunning(false);
    });
  };

  const sendCode = (file, fileName) => {
    socket.emit('code', file, fileName, (data) => {
      console.log(data);
      setCode((new TextDecoder("utf-8")).decode(data.file));
      setDebug(undefined);
      setFileName(data.fileName);
      setRunning(false);
    });
  };

  const sendQuery = (q) => {
    setRunning(true);
    let query = {...q, ...queries};
    console.log(query);
    socket.emit('query', query, (data) => {
      console.log(data);
      setDebug(data.data);
      setRunning(false);
    });
  }

  return (
    <div>
      <div style={{display: "flex"}}>
        <UploadCode sendCode={sendCode} fileName={fileName}/>
        <div>
          <p style={{margin: 0}}>Highlighting</p>
          <select defaultValue={"failures"} onChange={(event) => setCodeHighlight({...codeHighlight, "info": event.target.value})}>
            <option value={"encounters"}>Encounters</option>
            <option value={"failures"}>Failures</option>
            <option value={"failRatio"}>Failure Ratio</option>
            <option value={"successes"}>Successes</option>
            <option value={"successRatio"}>Success Ratio</option>
          </select>
        </div>
        <RunResume running={running} sendQuery={sendQuery} sendKill={sendKill} timeInfo={timeInfo}/>
      </div>
      <QueryPanel setQueries={setQueries}/>
      {/*<DebuggerPanel setDebug={setDebug} fileName={fileName}/>*/}
      {/*<select defaultValue={"color"} onChange={(event) => setCodeHighlight({...codeHighlight, "style": event.target.value})}>*/}
      {/*  <option value={"none"}>None</option>*/}
      {/*  <option value={"color"}>Color</option>*/}
      {/*  <option value={"bars"}>Bars</option>*/}
      {/*</select>*/}
      <div style={{display: "flex", flexDirection:"row", height: "calc(100vh - 20em)", width: "100vw"}}>
        <CodeContainer code={code} debug={debug} codeHighlight={codeHighlight} setPointDebug={setPointDebug}/>
        <div style={{width: "-webkit-fill-available"}}>
          {/*<TimeInfoPanel debug={debug}/>*/}
          <PointInfoPanel pointDebug={pointDebug} code={code}/>
          <SolutionInfoPanel debug={debug} code={code}/>
          <RejectionInfoPanel debug={debug} code={code}/>
        </div>
      </div>
    </div>
  );
}

export default App;
