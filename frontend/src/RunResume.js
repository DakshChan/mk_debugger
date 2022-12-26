import RunningIndicator from "./RunnningIndicator";
import {Button, ButtonGroup} from "@chakra-ui/react";

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
      <ButtonGroup size='sm' isAttached variant='outline'>
        <Button isLoading={running} onClick={executeRun} loadingText={"Running"}>Run</Button>
        <Button isDisabled={running} onClick={executeResume}>Resume</Button>
        <Button onClick={killProcess}>Kill</Button>
      </ButtonGroup>
      {/*<RunningIndicator running={running} debug={debug}/>*/}
      <div style={{display: "flex", gap: "1ch"}}>
        <p>{`Cpu: ${debug?.time?.cpu ?? 0}`}</p>
        <p>{`GC: ${debug?.time?.gc ?? 0}`}</p>
        <p>{`Real: ${debug?.time?.real ?? 0}`}</p>
      </div>
    </>
  );
}


