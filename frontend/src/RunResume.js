import RunningIndicator from "./RunnningIndicator";

export default function RunResume({sendQuery, sendKill, fileName, running, debug}) {
  const executeRun = (e) => {
    e.preventDefault();
    sendQuery({"command": "run"});
  }

  const executeResume = (e) => {
    e.preventDefault();
    sendQuery({"command": "resume"});
  }

  const killProcess = (e) => {
    e.preventDefault();
    sendKill();
  }

  return (
    <>
      <div style={{flex: 1, gap: "1ch"}}>
        <button onClick={executeRun}>Run</button>
        <button onClick={executeResume}>Resume</button>
        <button onClick={killProcess}>Kill</button>
      </div>
      <RunningIndicator running={running} debug={debug}/>
    </>
  );
}


