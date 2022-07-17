import "./InfoPanel.css"

export default function PointInfoPanel({pointDebug, code}) {

  if (pointDebug !== undefined) {
    return (
      <div className={"point-info-panel"}>
        <h3>Point Panel</h3>
        <div style={{display: "flex"}}>
          <p>{`Encounters: ${pointDebug.count}`}</p>
          <p>{`Failures: ${pointDebug.fails}`}</p>
          <p>{`Successes: ${pointDebug.successes}`}</p>
        </div>
        <div style={{display: "flex"}}>
          <p>Line: {pointDebug.syntax.line}</p>
          <p>Column: {pointDebug.syntax.column}</p>
        </div>
        <div>
            <p>Syntax: {code.substring(pointDebug.syntax.position - 1, pointDebug.syntax.position + pointDebug.syntax.span - 1)}</p>
        </div>
      </div>
    );
  } else {
    return (
      <div className={"point-info-panel"}>
        <h3>Point Panel</h3>
        <p>Run a query and select a point</p>
      </div>
    );
  }
}
