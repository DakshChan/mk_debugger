import './App.css';
import CodeContainer from "./CodeContainer";
import UploadCode from "./UploadCode";
import DebuggerPanel from "./DebuggerPanel";
import PointInfoPanel from "./PointInfoPanel";
import { useState, useEffect } from "react";

function App() {
  const [code, setCode] = useState("");
  const [debug, setDebug] = useState(undefined);
  const [codeHighlight, setCodeHighlight] = useState({"info": "encounters", "style": "color"});
  const [pointDebug, setPointDebug] = useState(undefined);

  return (
    <div>
      <div style={{display: "flex", flexDirection:"row", alignItems:"center"}}>
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
        <select defaultValue={"color"} onChange={(event) => setCodeHighlight({...codeHighlight, "style": event.target.value})}>
          <option value={"none"}>None</option>
          <option value={"color"}>Color</option>
          <option value={"bars"}>Bars</option>
        </select>
        <CodeContainer code={code} debug={debug} codeHighlight={codeHighlight} setPointDebug={setPointDebug}></CodeContainer>
        <PointInfoPanel pointDebug={pointDebug}/>
        <></>
      </div>
    </div>
  );
}

export default App;
