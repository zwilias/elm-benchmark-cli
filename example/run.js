const Elm = require("./elm.js");

const app = Elm.Main.worker();

app.ports.emit.subscribe(function(v) {
  switch (v.type) {
    case "start":
      console.log(v.data);
      process.stdout.write("\x1B[?25l");
      break;

    case "running":
      process.stdout.write(v.data);
      break;

    case "done":
      process.stdout.write("\x1B[?25h\n\n");
      console.log(v.data);
      process.exit(0);
  }
});
