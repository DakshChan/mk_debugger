import "./InfoPanel.css"

export default function TimeInfoPanel({debug}) {

  if (debug !== undefined) {
    return (
      <div className={"time-info-panel"}>
        <h3>Time Panel</h3>
        <div style={{display: "flex"}}>
          <p>{`Cpu: ${debug.time.cpu}`}</p>
          <p>{`GC: ${debug.time.gc}`}</p>
          <p>{`Real: ${debug.time.real}`}</p>
        </div>
      </div>
    );
  } else {
    return (
      <div className={"time-info-panel"}>
        <h3>Time Panel</h3>
        <p>Run a query</p>
      </div>
    );
  }
}
