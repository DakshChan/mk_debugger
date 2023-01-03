import { Button } from "@chakra-ui/react";

export default function UploadCode({sendCode, fileName}) {
  function handleChange(event) {

    const file = event.target.files[0];
    console.log(file);
    sendCode(file, file.name);
    event.target.value = '';
    event.target.placeholder = 'Upload code';
  }

  return (
    <>
      <label htmlFor="file_upload">
        <Button size={"sm"} style={{pointerEvents: "none"}} variant='outline' >Choose file</Button>
        <span style={{fontSize:"small", marginInline:"1ch"}}>
          {fileName !== "" ? fileName : "No file chosen"}
        </span>
      </label>
      <input
        style={{opacity: 0, width:0, height:0}}
        id="file_upload" type="file" onChange={handleChange}
      />
    </>
  )
}
