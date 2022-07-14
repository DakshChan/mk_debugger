import "./InfoPanel.css"
import StateInfoPanel from "./StateInfoPanel";
import {useState} from "react";

export default function RejectionInfoPanel({debug, code}) {
  const [state, setState] = useState(undefined);

  function handleStateSelect(e) {
    if (e.target.value === "") {
      setState(undefined)
      return;
    }
    let n = parseInt(e.target.value);
    if (n < 0 || n >= debug["rejected-states"].length) {
      setState(undefined);
      return;
    }
    setState(debug["rejected-states"][n]);
  }

  if (debug !== undefined) {
    return (
      <div className={"rejection-info-panel"}>
        <h3>Rejection panel</h3>
        {
          debug["rejected-states"].length > 0 ?
            <>
              <p>{"0-" + (debug["rejected-states"].length - 1)}</p>
              <input type={"number"} min={0} max={debug["rejected-states"].length - 1}
                     placeholder={"0-" + (debug["rejected-states"].length - 1)} onInput={handleStateSelect}/>
              {state !== "" ? <StateInfoPanel state={state} code={code}/> : <p>Select state</p>}
            </> :
            <p>No solutions</p>
        }
      </div>
    );
  } else {
    return (
      <div className={"rejection-info-panel"}>
        <h3>Rejection panel</h3>
        <p>Run a query</p>
      </div>
    );
  }
}
