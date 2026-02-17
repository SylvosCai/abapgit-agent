# Installation & Setup

## Architecture

```
Claude (VS Code) → CLI Tool → ABAP System (REST/HTTP)
                                  ↓
                    Result + Error Feedback
```

## Components

### ABAP Side - REST Implementation

The ABAP backend consists of:
- **Main agent** - Core logic for pull, inspect, unit operations
- **REST handler** - Routes HTTP requests to appropriate resources
- **Resource handlers** - Individual endpoints (/pull, /inspect, /unit, /tree, /view, /preview, /health)

Deploy all ABAP objects using abapGit from the `/abap` folder.

### CLI Tool (Node.js)
- REST client for ABAP communication
- Configuration management (reads from cwd/.abapGitAgent)
- Auto-detects git remote URL and branch from current directory
- Cookie-based session management

## Why REST API?

The REST API approach provides several advantages:
- **No OData/Gateway needed** - Simpler setup
- **Standard HTTP** - Easy to debug and test
- **No SEGW required** - Direct ICF handler
- **No native dependencies** - Works on any OS

## ABAP System Setup

### Prerequisites

The developer version of abapGit is required to deploy the REST handler. Follow the [abapGit developer version installation guide](https://docs.abapgit.org/user-guide/getting-started/install.html#install-developer-version).

### Deploy ABAP Objects

Deploy ABAP objects using abapGit:

1. Use abapGit to deploy the ABAP objects in `/abap` folder
2. Create SICF handler:
   - Run transaction `SICF`
   - Navigate to: `sap/bc/z_abapgit_agent`
   - Create if doesn't exist:
     - Right-click on `sap/bc` → **Create Element**
     - **Service Name**: `Z_ABAPGIT_AGENT`
     - **Handler Class**: `ZCL_ABGAGT_REST_HANDLER`
   - Activate the service

## CLI Installation

```bash
# Option A: Install globally for system-level integration
sudo npm install -g abapgit-agent

# Option B: For local development, use npm link instead
cd abapgit-agent
sudo npm link
```

## Configuration

Copy `.abapGitAgent.example` to `.abapGitAgent` in your repository root:

```bash
cp .abapGitAgent.example .abapGitAgent
```

### Configuration Options

**File-based** (.abapGitAgent):
```json
{
  "host": "your-sap-system.com",
  "sapport": 443,
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "language": "EN",
  "gitUsername": "github-username",
  "gitPassword": "github-token"
}
```

**Environment variables**:
- `ABAP_HOST`, `ABAP_PORT`, `ABAP_CLIENT`, `ABAP_USER`, `ABAP_PASSWORD`

## System-Level Integration

This package supports **system-level integration**, meaning any ABAP git repository can use it without cloning this repository:

1. **Install globally**: `sudo npm install -g abapgit-agent`
2. **Configure**: Add `.abapGitAgent` to your ABAP repo
3. **Use**: Run `abapgit-agent` from your repo directory

## Creating New ABAP Projects

### 1. Create Project Structure

```bash
# Create new ABAP repository
mkdir my-abap-repo
cd my-abap-repo

# Copy CLAUDE.md (tells Claude how to work with ABAP)
cp /path/to/abapgit-agent/abap/CLAUDE.md .

# Initialize git
git init
```

### 2. Add ABAP Objects

Copy your ABAP objects to the repo:

```bash
# Copy individual objects
cp /path/to/abapgit-agent/abap/zcl_*.abap ./
```

### 3. Configure and Push

```bash
# Create .abapGitAgent config
cp .abapGitAgent.example .abapGitAgent
# Edit .abapGitAgent with your SAP system details

# Initial commit
git add .
git commit -m "Initial ABAP project"

# Add remote and push
git remote add origin https://github.com/user/my-abap-repo.git
git push -u origin main
```

### 4. Deploy to SAP

Use abapGit in your SAP system to pull the repository.

## Claude Code Integration

Claude Code automatically reads `CLAUDE.md` in your project root for context:

1. **No local syntax validation** - Claude knows ABAP needs SAP system
2. **Use `abapgit-agent pull`** - For validation after code generation
3. **Reference local docs** - See CLAUDE_MEM.md for knowledge management

See `abap/CLAUDE.md` for detailed ABAP project guidelines.
