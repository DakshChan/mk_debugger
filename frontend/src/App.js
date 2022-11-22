import CodeContainer from "./CodeContainer";
import UploadCode from "./UploadCode";
import PointInfoPanel from "./PointInfoPanel";
import { useState } from "react";
import './App.css';
import SolutionInfoPanel from "./SolutionInfoPanel";
import RejectionInfoPanel from "./RejectionInfoPanel";
import TimeInfoPanel from "./TimeInfoPanel";
import RunResume from "./RunResume";
import QueryForm from "./QueryForm";

function App() {
  const [code, setCode] = useState("");
  const [debug, setDebug] = useState(undefined);
  const [codeHighlight, setCodeHighlight] = useState({"info": "encounters", "style": "color"});
  const [pointDebug, setPointDebug] = useState(undefined);
  const [fileName, setFileName] = useState("");

  return (
    <div>
      <div style={{display: "flex"}}>
        <UploadCode setCode={setCode} setDebug={setDebug} fileName={fileName} setFileName={setFileName}/>
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
        <RunResume/>
      </div>
      <QueryForm/>
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
