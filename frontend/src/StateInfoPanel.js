import "./InfoPanel.css"
import Collapsible from "react-collapsible";
import {useEffect, useState} from "react";

export default function StateInfoPanel({state, code}) {
  const [path, setPath] = useState(undefined);
  const [stack, setStack] = useState(undefined);
  const [failed, setFailed] = useState(undefined);
  const [binding, setBinding] = useState(undefined);

  useEffect(() => {
    if (state !== undefined) {
      setPath(state.path);
      setStack(state.stack);
      setFailed(state.failed);
      setBinding(state.binding);
    }
  }, [state]);

  return (
    <div className={"state-info-panel"}>
      <h4>Term</h4>
      <p>{binding?.sub}</p>
      <h4>Constraints</h4>
      <p>{binding?.cxs}</p>
      {
        failed === undefined ? <></> :
          <>
            <h4>Failed Goal</h4>
            <p>{failed?.sub}</p>
            <h4>Failed Constraints</h4>
            <p>{failed?.cxs}</p>
          </>
      }
      {
        (path === undefined || path.length === 0) ? <></> :
        <Collapsible trigger={<h4>> Path</h4>} triggerWhenOpen={<h4>v Path</h4>}>
          <p>{path.length}</p>
          {path.slice(0).reverse().map((s, index) => {
            return <p key={index}>{`${s.line !== false ? s.line + ":" + s.column : ""} ${s.content}`}</p>;
          })}
        </Collapsible>
      }
      {
        (stack === undefined || stack.length === 0) ? <></> :
        <Collapsible trigger={<h4>> Stack</h4>} triggerWhenOpen={<h4>v Stack</h4>}>
          <p>{stack.length}</p>
          {stack.slice(0).reverse().map((s, index) => {
            return <p key={index}>{`${s.line !== false ? s.line + ":" + s.column : ""} ${s.content}`}</p>;
          })}
        </Collapsible>
      }
    </div>
  )
}
