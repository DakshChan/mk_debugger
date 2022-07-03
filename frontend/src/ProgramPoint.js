export default function ProgramPoint({children, data}) {

  return (
    <span className="program-point" style={{background: "red"}}>
      {children}
    </span>
  );
}
