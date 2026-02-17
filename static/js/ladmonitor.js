/* MBLogic-CL Ladder Monitor
   Loads ladder diagram from API and displays it
*/

var LadDisplay = null;
var CurrentSubroutine = "main";

// Initialize on page load
document.addEventListener("DOMContentLoaded", function() {
    console.log("Ladder Monitor: Initializing...");

    // Initialize the display library
    // First create the symbols definitions from the document
    var ladsymbols = new LadSymDefs(document);
    // Then create the display control with the symbols
    LadDisplay = new SubrDispControl(document, ladsymbols);

    console.log("Display control initialized");

    // Load subroutine list
    loadSubroutineList();

    // Load initial ladder diagram
    loadLadderDiagram("main");

    // Setup event handlers
    var subrSelect = document.getElementById("subr-select");
    if (subrSelect) {
        subrSelect.addEventListener("change", function(e) {
            CurrentSubroutine = e.target.value;
            loadLadderDiagram(CurrentSubroutine);
        });
    }

    var btnStart = document.getElementById("btn-start");
    if (btnStart) btnStart.addEventListener("click", startPLC);

    var btnStop = document.getElementById("btn-stop");
    if (btnStop) btnStop.addEventListener("click", stopPLC);

    var btnStep = document.getElementById("btn-step");
    if (btnStep) btnStep.addEventListener("click", stepPLC);

    var btnMonitor = document.getElementById("btn-monitor");
    if (btnMonitor) btnMonitor.addEventListener("click", updateMonitor);
});

function loadSubroutineList() {
    console.log("Loading subroutine list...");
    fetch("/api/subroutines")
        .then(response => response.json())
        .then(data => {
            const selectElement = document.getElementById("subr-select");
            selectElement.innerHTML = "";
            
            data.subroutines.forEach(name => {
                const option = document.createElement("option");
                option.value = name;
                option.textContent = name;
                selectElement.appendChild(option);
            });
            
            console.log("Loaded " + data.subroutines.length + " subroutines");
        })
        .catch(error => console.error("Error loading subroutines:", error));
}

function loadLadderDiagram(subroutineName) {
    console.log("Loading ladder diagram for: " + subroutineName);
    
    fetch("/api/ladder-js?subrname=" + encodeURIComponent(subroutineName))
        .then(response => response.json())
        .then(data => {
            console.log("Received rung data: " + data.rungdata.length + " rungs");
            
            // Clear existing rungs
            const container = document.getElementById("staticrunglist");
            container.innerHTML = "";
            
            // Add subroutine name and comments
            const subheader = document.createElement("h2");
            subheader.textContent = "SBR " + data.subroutinename;
            container.appendChild(subheader);
            
            if (data.subrcomments) {
                const comments = document.createElement("p");
                comments.textContent = data.subrcomments;
                container.appendChild(comments);
            }
            
            // Create rungs using LadDisplay
            LadDisplay.CreateRungList(data);
            
            console.log("Ladder diagram loaded successfully");
        })
        .catch(error => {
            console.error("Error loading ladder diagram:", error);
            document.getElementById("staticrunglist").innerHTML = 
                "<p style='color: red;'>Error loading ladder diagram: " + error + "</p>";
        });
}

function startPLC() {
    console.log("Starting PLC...");
    fetch("/api/control/start", { method: "POST" })
        .then(response => response.json())
        .then(data => {
            console.log("PLC started:", data);
            updateMonitor();
        });
}

function stopPLC() {
    console.log("Stopping PLC...");
    fetch("/api/control/stop", { method: "POST" })
        .then(response => response.json())
        .then(data => {
            console.log("PLC stopped:", data);
            updateMonitor();
        });
}

function stepPLC() {
    console.log("Single step PLC...");
    fetch("/api/control/step", { method: "POST" })
        .then(response => response.json())
        .then(data => {
            console.log("PLC stepped:", data);
            updateMonitor();
        });
}

function updateMonitor() {
    fetch("/api/statistics")
        .then(response => response.json())
        .then(data => {
            document.getElementById("status-running").textContent = data.running ? "Running" : "Stopped";
            document.getElementById("status-running").className = 
                "status-value " + (data.running ? "running" : "stopped");
            document.getElementById("status-scan-count").textContent = data.scan_count || 0;
            document.getElementById("status-scan-time").textContent = 
                ((data.scan_time || 0) / 1000).toFixed(2) + " ms";
        });
}
