import "./InfoPanel.css"
import md5 from "md5";
import Collapsible from "react-collapsible";

export default function StateInfoPanel({state, code}) {

  if (state !== undefined) {
    const {path, stack, failed, binding} = state;
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
        <Collapsible trigger={<h4>> Path</h4>} triggerWhenOpen={<h4>v Path</h4>}>
          {path.map((s, index) => {
            return <p key={md5(s.line + index + "path")}>{`${s.line}:${s.column} ${code.substring(s.position - 1, s.position + s.span - 1)}`}</p>;
          })}
        </Collapsible>
        <Collapsible trigger={<h4>> Stack</h4>} triggerWhenOpen={<h4>v Stack</h4>}>
          {stack.map((s, index) => {
            return <p key={md5(s.line + index + "stack")}>{`${s.line}:${s.column} ${code.substring(s.position - 1, s.position + s.span - 1)}`}</p>;
          })}
        </Collapsible>
      </div>
    )
  } else {
    return (
      <div className={"state-info-panel"}>
        Invalid state selected
      </div>
    );
  }
}
