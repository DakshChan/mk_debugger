export default function QueryForm() {
  return (
    <div style={{display: "flex"}}>
      <p style={{margin: "0 0.2em", fontSize:"1.3em"}}>( run </p>
      <div>
        <div>
          <input placeholder={"num solutions"}/>
        </div>
        <div>
          <input placeholder={"samples"}/>
        </div>
        <div>
          <input placeholder={"step"}/>
        </div>
        <div>
          <input placeholder={"query vars"}/>
        </div>
      </div>
      <textarea placeholder={"query"}/>
      <p style={{margin: "0 0.2em", fontSize:"1.3em"}}>)</p>
    </div>
  );
}