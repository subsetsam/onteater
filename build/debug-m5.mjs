import { chromium } from "playwright-core";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { readFileSync } from "node:fs";
import { createServer } from "node:http";
const __dirname = dirname(fileURLToPath(import.meta.url));
const html = readFileSync(resolve(__dirname, "../dist/onteater.html"), "utf8");
const ONTO = resolve(__dirname, "../examples/galactic-economic-ontology.json");
const SCEN = readFileSync(resolve(__dirname, "../docs/sample-scenario.md"), "utf8");
const server = createServer((_r, res) => { res.writeHead(200, {"Content-Type":"text/html"}); res.end(html); });
await new Promise((r)=>server.listen(0,"127.0.0.1",r));
const url = `http://127.0.0.1:${server.address().port}/`;
const browser = await chromium.launch();
const page = await browser.newPage();
page.on("console", (m)=>console.log("PAGE:", m.text().slice(0,300)));
page.on("pageerror",(e)=>console.log("PAGEERR:", e.message));
page.on("requestfailed",(r)=>console.log("REQFAIL:", r.url(), r.failure()?.errorText));
page.on("response", async (r)=>{ if (r.url().includes("/api/chat")) console.log("CHAT RESP status", r.status()); });
await page.addInitScript(()=>{ try{delete window.showOpenFilePicker;delete window.showSaveFilePicker;}catch(_){}} );
await page.goto(url,{waitUntil:"load"});
const [ch]=await Promise.all([page.waitForEvent("filechooser"),page.click("button.btn-primary")]);
await ch.setFiles(ONTO);
await page.waitForSelector(".graph-canvas g.node");
await page.click(".menubar-actions .icon-btn[title='Ollama settings']");
await page.click(".settings-row .btn-primary");
await page.waitForSelector(".conn-ok",{timeout:12000});
await page.selectOption(".settings-dialog select","gemma4:e4b");
await page.click(".dialog-actions .btn");
await page.click(".workspace-tabs .tab:has-text('Scenario')");
await page.waitForSelector(".scenario-input");
await page.fill(".scenario-input", SCEN);
await page.click(".scenario-actions .btn-primary");
// poll run status for 90s
for (let i=0;i<18;i++){
  await page.waitForTimeout(5000);
  const st = await page.evaluate(()=>{
    const p = document.querySelector(".run-progress")?.textContent;
    const cards = document.querySelectorAll(".entry-card").length;
    const empty = document.querySelector(".board-empty")?.textContent;
    return {p, cards, empty: empty?.slice(0,80)};
  });
  console.log(`t+${(i+1)*5}s`, JSON.stringify(st));
  if (st.cards>0) { console.log("GOT ENTRIES"); break; }
}
await browser.close(); server.close();
