import CodeContainer from "./CodeContainer";
import UploadCode from "./UploadCode";
import DebuggerPanel from "./DebuggerPanel";
import PointInfoPanel from "./PointInfoPanel";
import { useState, useEffect } from "react";
import './App.css';
import SolutionInfoPanel from "./SolutionInfoPanel";
import RejectionInfoPanel from "./RejectionInfoPanel";

function App() {
  const [code, setCode] = useState("");
  const [debug, setDebug] = useState(undefined);
  const [codeHighlight, setCodeHighlight] = useState({"info": "encounters", "style": "color"});
  const [pointDebug, setPointDebug] = useState(undefined);

  return (
    <div>
      <div>
        <UploadCode setCode={setCode}/>
        <DebuggerPanel setDebug={setDebug}/>
      </div>
      <div>
        <div onChange={(event) => setCodeHighlight({...codeHighlight, "info": event.target.value})}>
          <label>Encounters</label>
          <input type={"radio"} name={"PP-Select"} defaultChecked={true} value={"encounters"}/>
          <label>Failures</label>
          <input type={"radio"} name={"PP-Select"} value={"failures"}/>
        </div>
        {/*<select defaultValue={"color"} onChange={(event) => setCodeHighlight({...codeHighlight, "style": event.target.value})}>*/}
        {/*  <option value={"none"}>None</option>*/}
        {/*  <option value={"color"}>Color</option>*/}
        {/*  <option value={"bars"}>Bars</option>*/}
        {/*</select>*/}
        <div style={{display: "flex", flexDirection:"row", height: "calc(100vh - 6em)", width: "100vw"}}>
          <CodeContainer code={code} debug={debug} codeHighlight={codeHighlight} setPointDebug={setPointDebug}/>
          <div style={{width: "-webkit-fill-available"}}>
            <PointInfoPanel pointDebug={pointDebug}/>
            <SolutionInfoPanel debug={debug}/>
            <RejectionInfoPanel debug={debug}/>
          </div>
        </div>
      </div>
    </div>
  );
}

export default App;
