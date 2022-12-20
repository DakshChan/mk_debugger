import "./InfoPanel.css"
import StateInfoPanel from "./StateInfoPanel";
import {useEffect, useState} from "react";
import md5 from "md5";

export default function SolutionInfoPanel({debug, code}) {
  const [state, setState] = useState(undefined);
  const [solution, setSolution] = useState(undefined);
  const [inputIndex, setInputIndex] = useState("");

  useEffect(() => {
    if (debug !== undefined) {
      setSolution(debug.solutions);
    } else {
      setSolution(undefined);
    }
    setState(undefined);
    setInputIndex("");
  }, [debug]);

  useEffect(() => {
    setState(undefined);
    setSolution(undefined);
    setInputIndex("");
  }, [code]);

  useEffect(() => {
    if (inputIndex === "") {
      setState(undefined)
      return;
    }
    if (solution === undefined) {
      setState(undefined);
      return;
    }
    let n = parseInt(inputIndex);
    if (n < 0 || n >= solution.length) {
      setState(undefined);
      return;
    }
    setState(solution[n]);
  }, [inputIndex]);

  return (
    <div className={"solution-info-panel"}>
      <h3>Solution panel</h3>
      {
        solution !== undefined ?
        <>
          {
            solution.length > 0 ?
              <>
                <p>{"Enter a number from 0 -> " + (solution.length - 1)}</p>
                <input type={"number"} min={0} max={solution.length - 1}
                       placeholder={"0 -> " + (solution.length - 1)} value={inputIndex}
                       onChange={event => setInputIndex(event.target.value)} />
                {
                  (inputIndex !== "" && (parseInt(inputIndex) < 0 || parseInt(inputIndex) > (solution.length - 1))) ?
                    <p>Invalid Number</p> :
                    ((state !== undefined) ?
                        <StateInfoPanel state={state} code={code}/> :
                        <></>
                    )
                }
              </> :
              <p>No solutions</p>
          }
        </> :
        <p>Run a query</p>
      }
    </div>
  );
}
