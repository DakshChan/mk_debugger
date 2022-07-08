import {useEffect} from "react";
import "./InfoPanel.css"

export default function PointInfoPanel({pointDebug}) {

  if (pointDebug !== undefined) {
    return (
      <div className={"point-info-panel"}>
        <h3>Point Panel</h3>
        <p>Encounters: {pointDebug.count}</p>
        <p>Failures: {pointDebug.fails}</p>
        <p>Line: {pointDebug.syntax.line}</p>
        <p>Column: {pointDebug.syntax.column}</p>
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
