import "./InfoPanel.css"

export default function PointInfoPanel({pointDebug, code}) {

  if (pointDebug !== undefined) {
    return (
      <div className={"point-info-panel"}>
        <h3>Point Panel</h3>
        <div style={{display: "flex", gap: "3ch"}}>
          <p>Encounters: {pointDebug.count}</p>
          <p>Failures: {pointDebug.fails}</p>
          <p>Successes: {pointDebug.successes}</p>
        </div>
        <div style={{display: "flex", gap: "3ch"}}>
          <p>Line: {pointDebug.syntax.line}</p>
          <p>Column: {pointDebug.syntax.column}</p>
        </div>
        <div>
            <p>Syntax: {pointDebug.syntax.content}</p>
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
