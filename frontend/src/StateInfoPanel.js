import "./InfoPanel.css"

export default function StateInfoPanel({state}) {
  if (state !== undefined) {
    return (
      <div className={"state-info-panel"}>
        <p>State display here</p>
        <p>{JSON.stringify({
          ...state, path: state.path.map((a) => {
            const {source, ...rest} = a;
            return rest
          })
        }, null, 4)}</p>
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
