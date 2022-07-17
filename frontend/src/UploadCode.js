import axios from "axios";

export default function UploadCode({setCode, setDebug}) {
  function handleChange(event) {
    const file = event.target.files[0];
    console.log(file);
    const url = 'http://localhost:3000/code';
    const formData = new FormData();
    formData.append('file', file);
    formData.append('fileName', file.name);
    const config = {
      headers: {
        'content-type': 'multipart/form-data',
      },
    };
    axios.post(url, formData, config).then((response) => {
      console.log(response.status);
      setCode(response.data);
      setDebug(undefined);
    }).catch((error) => {
      console.log(error);
    });
  }

  return (
    <>
      <input type="file" onChange={handleChange}/>
    </>
  )
}
