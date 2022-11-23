import "./InfoPanel.css"
import StateInfoPanel from "./StateInfoPanel";
import {useEffect, useState} from "react";

export default function RejectionInfoPanel({debug, code}) {
  const [state, setState] = useState(undefined);
  const [rejection, setRejection] = useState(undefined);

  useEffect(() => {
    if (debug !== undefined) {
      setRejection(debug["rejected-states"]);
    } else {
      setRejection(undefined);
      setState(undefined);
    }
  }, [debug]);

  useEffect(() => {
    setState(undefined);
    setRejection(undefined);
  }, [code]);

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
    if (rejection === undefined) {
      setState(undefined);
      return;
    }
    setState(debug["rejected-states"][n]);
  }

    return (
      <div className={"rejection-info-panel"}>
        <h3>Rejection panel</h3>
        {rejection !== undefined ?
          <>
            {
              rejection.length > 0 ?
                <>
                  <p>{"0-" + (rejection.length - 1)}</p>
                  <input type={"number"} min={0} max={rejection.length - 1}
                         placeholder={"0-" + (rejection.length - 1)} onInput={handleStateSelect}/>
                  {state !== undefined ? <StateInfoPanel state={state} code={code}/> : <p>Select state</p>}
                </> :
                <p>No solutions</p>
            }
          </> :
          <p>Run a query</p>
        }
      </div>
    );
}
