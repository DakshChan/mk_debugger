import "./InfoPanel.css"
import md5 from "md5";

export default function StateInfoPanel({state, code}) {

  if (state !== undefined) {
    const {sub, diseq, types, distypes, path, stack} = state;
    return (
      <div className={"state-info-panel"}>
        <p>State display here</p>
        <p>sub</p>
        <p>{JSON.stringify(sub)}</p>
        <p>diseq</p>
        <p>{JSON.stringify(diseq)}</p>
        <p>types</p>
        <p>{JSON.stringify(types)}</p>
        <p>distypes</p>
        <p>{JSON.stringify(distypes)}</p>
        <p>path</p>
        {path.map((s, index) => {
          return <p key={md5(s.line + index + "path")}>{`${s.line}:${s.column} ${code.substring(s.position - 1, s.position + s.span - 1)}`}</p>;
        })}
        <p>stack</p>
        {stack.map((s, index) => {
          return <p key={md5(s.line + index + "stack")}>{`${s.line}:${s.column} ${code.substring(s.position - 1, s.position + s.span - 1)}`}</p>;
        })}
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
