import "./InfoPanel.css"

export default function StateInfoPanel({state, code}) {

  if (state !== undefined) {
    const {failed, binding} = state;
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
