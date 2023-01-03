import { Button, ButtonGroup } from "@chakra-ui/react";
import { useEffect, useState } from "react";

export default function RunResume({sendQuery, sendKill, status, debug}) {
  const [running, setRunning] = useState(false);

  useEffect(() => {
    if (status === "running") {
      setRunning(true);
    } else {
      setRunning(false);
    }
  }, [status]);

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
    <div style={{display: "flex", gap: "1ch", alignItems: "center"}}>
      <ButtonGroup size='sm' isAttached variant='outline'>
        <Button isLoading={running} onClick={executeRun} loadingText={"Running"}>Run</Button>
        <Button isDisabled={running} onClick={executeResume}>Resume</Button>
        <Button onClick={killProcess}>Kill</Button>
      </ButtonGroup>
      <div style={{display: "flex", gap: "1ch"}}>
        <p>{`Cpu: ${debug?.time?.cpu ?? 0}`}</p>
        <p>{`GC: ${debug?.time?.gc ?? 0}`}</p>
        <p>{`Real: ${debug?.time?.real ?? 0}`}</p>
      </div>
    </div>
  );
}


