import './App.css';
import CodeContainer from "./CodeContainer";

function App() {

  const code = `<span class="highlight-inline" onmouseover="alert('test')">(require racket/syntax)</span>
(syntax (+ 2 (- 2 3)))`;

  return (
    <>
      <CodeContainer code={code}></CodeContainer>
    </>
  );
}

export default App;
