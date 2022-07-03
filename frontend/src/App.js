import './App.css';
import CodeContainer from "./CodeContainer";
import UploadCode from "./UploadCode";
import DebuggerPanel from "./DebuggerPanel";
import { useState, useEffect } from "react";

function App() {
  const [code, setCode] = useState('');
  const [debug, setDebug] = useState();

  return (
    <>
      <UploadCode setCode={setCode}/>
      <DebuggerPanel setDebug={setDebug}/>
      <CodeContainer code={code} debug={debug}></CodeContainer>
    </>
  );
}

export default App;
