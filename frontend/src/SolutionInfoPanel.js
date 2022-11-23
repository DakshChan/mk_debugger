import "./InfoPanel.css"
import StateInfoPanel from "./StateInfoPanel";
import {useEffect, useState} from "react";
import md5 from "md5";

export default function SolutionInfoPanel({debug, code}) {
  const [state, setState] = useState(undefined);
  const [solution, setSolution] = useState(undefined);

  useEffect(() => {
    if (debug !== undefined) {
      setSolution(debug.solutions);
    } else {
      setSolution(undefined);
      setState(undefined);
    }
  }, [debug]);

  useEffect(() => {
    setState(undefined);
    setSolution(undefined);
  }, [code]);

  function handleStateSelect(e) {
    if (e.target.value === "") {
      setState(undefined)
      return;
    }
    let n = parseInt(e.target.value);
    if (n < 0 || n >= solution.length) {
      setState(undefined);
      return;
    }
    if (solution === undefined) {
      setState(undefined);
      return;
    }
    setState(solution[n]);
  }

  return (
    <div className={"solution-info-panel"}>
      <h3>Solution panel</h3>
      {
        solution !== undefined ?
        <>
          {
            solution.length > 0 ?
              <>
                <p>{"0-" + (solution.length - 1)}</p>
                <input type={"number"} min={0} max={solution.length - 1}
                       placeholder={"0-" + (solution.length - 1)} onInput={handleStateSelect} />
                {state !== undefined ? <StateInfoPanel key={md5(JSON.stringify(solution) + "div" + JSON.stringify(state))} state={state} code={code}/> : <p>Select state</p>}
              </> :
              <p>No solutions</p>
          }
        </> :
        <>
          <p>Run a query</p>
        </>
      }
    </div>
  );
}
