import "./InfoPanel.css"
import StateInfoPanel from "./StateInfoPanel";
import {useState} from "react";

export default function SolutionInfoPanel({debug}) {
  const [state, setState] = useState(undefined);

  function handleStateSelect(e) {
    if (e.target.value === "") {
      setState(undefined)
      return;
    }
    let n = parseInt(e.target.value);
    if (n < 0 || n >= debug.solutions.length) {
      setState(undefined);
      return;
    }
    setState(debug.solutions[n]);
  }

  if (debug !== undefined) {
    return (
      <div className={"solution-info-panel"}>
        <h3>Solution panel</h3>
        {
          debug.solutions.length > 0 ?
            <>
              <p>{"0-" + (debug.solutions.length - 1)}</p>
              <input type={"number"} min={0} max={debug.solutions.length - 1}
                     placeholder={"0-" + (debug.solutions.length - 1)} onInput={handleStateSelect}/>
              {state !== "" ? <StateInfoPanel state={state}/> : <p>Select state</p>}
            </> :
            <p>No solutions</p>
        }
      </div>
    );
  } else {
    return (
      <div className={"solution-info-panel"}>
        <h3>Solution panel</h3>
        <p>Run a query</p>
      </div>
    );
  }
}
