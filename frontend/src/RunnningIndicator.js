import {MoonLoader} from 'react-spinners';
import {useEffect, useState} from "react";
import {useStopwatch} from "react-timer-hook";

export default function RunningIndicator({running, timeInfo}) {
  const [loading, setLoading] = useState(false);

    const {seconds, minutes, hours, days, isRunning, start, pause, reset} = useStopwatch({ autoStart: false });

  useEffect(() => {
    if (running) {
      setLoading(true);
      start();
    } else {
      setLoading(false);
      reset(null, false);
    }
  }, [running]);
  return (
    <div className={"running-indicator"}>
      {loading ?
        <div style={{position:"relative"}}>
          <MoonLoader color="#36d7b7" size={"50px"}/>
          <p style={{position: "absolute", margin:0, left: "50%", top: "50%", transform: "translate(-50%, -50%)"}}>{minutes}:{seconds}</p>
        </div> :
        <div style={{display: "flex"}}>
          <p>{`Cpu: ${timeInfo?.time?.cpu ?? 0}`}</p>
          <p>{`GC: ${timeInfo?.time?.gc ?? 0}`}</p>
          <p>{`Real: ${timeInfo?.time?.real ?? 0}`}</p>
        </div>
      }
    </div>
  );
}