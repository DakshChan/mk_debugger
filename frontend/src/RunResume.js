import "./DebuggerPanel.css";
import RunningIndicator from "./RunnningIndicator";

export default function RunResume({sendQuery, sendKill, fileName, running, timeInfo}) {

  // const executeQuery = (query) => {
  //   console.log(query);
  //   if (fileName !== "") setQueryCache({...queryCache, [fileName]:query});
  //   let solutions = parseInt(query.solutions);
  //   solutions = solutions > -1 ? solutions : "#f";
  //   let samples = parseInt(query.samples);
  //   samples = samples > -1 ? samples : "#f";
  //   let steps = parseInt(query.steps);
  //   steps = steps > -1 ? steps : "#f";
  //   query.solutions = solutions.toString();
  //   query.samples = samples.toString();
  //   query.steps = steps.toString();
  //   console.log(query);
  //   sendQuery(query);
  // }

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
      <form className={"panelForm"} key={fileName}>
        <button onClick={executeRun}>Run</button>
        <button onClick={executeResume}>Resume</button>
        <button onClick={killProcess}>Kill</button>
      </form>
      <RunningIndicator running={running} timeInfo={timeInfo}/>
    </>
  );
}


