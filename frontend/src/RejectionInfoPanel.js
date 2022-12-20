import "./InfoPanel.css"
import StateInfoPanel from "./StateInfoPanel";
import {useEffect, useState} from "react";

export default function RejectionInfoPanel({debug, code}) {
  const [state, setState] = useState(undefined);
  const [rejection, setRejection] = useState(undefined);
  const [inputIndex, setInputIndex] = useState("");

  useEffect(() => {
    if (debug !== undefined) {
      setRejection(debug["rejected-states"]);
    } else {
      setRejection(undefined);
    }
    setState(undefined);
    setInputIndex("");
  }, [debug]);

  useEffect(() => {
    setState(undefined);
    setRejection(undefined);
    setInputIndex("");
  }, [code]);

  useEffect(() => {
    if (inputIndex === "") {
      setState(undefined)
      return;
    }
    if (rejection === undefined) {
      setState(undefined);
      return;
    }
    let n = parseInt(inputIndex);
    if (n < 0 || n >= rejection.length) {
      setState(undefined);
      return;
    }
    setState(rejection[n]);
  }, [inputIndex]);

    return (
      <div className={"rejection-info-panel"}>
        <h3>Rejection panel</h3>
        {
          rejection !== undefined ?
          <>
            {
              rejection.length > 0 ?
                <>
                  <p>{"Enter a number from 0 -> " + (rejection.length - 1)}</p>
                  <input type={"number"} min={0} max={rejection.length - 1}
                         placeholder={"0 -> " + (rejection.length - 1)}
                         onChange={event => setInputIndex(event.target.value)}/>
                  {
                    (inputIndex !== "" && (parseInt(inputIndex) < 0 || parseInt(inputIndex) > (rejection.length - 1))) ?
                      <p>Invalid Number</p> :
                      ((state !== undefined) ?
                          <StateInfoPanel state={state} code={code}/> :
                          <></>
                      )
                  }
                </> :
                <p>No rejections</p>
            }
          </> :
          <p>Run a query</p>
        }
      </div>
    );
}
