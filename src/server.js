/**
 * HTTP Server for Claude Integration
 */

const express = require('express');
const cors = require('cors');
const { ABAPGitAgent } = require('./agent');
const { getAgentConfig } = require('./config');
const logger = require('./logger');

class Server {
  constructor() {
    this.app = express();
    this.agent = new ABAPGitAgent();
    this.agentConfig = getAgentConfig();

    this.setupMiddleware();
    this.setupRoutes();
  }

  setupMiddleware() {
    this.app.use(cors());
    this.app.use(express.json());

    // Request logging
    this.app.use((req, res, next) => {
      logger.debug(`${req.method} ${req.path}`, { body: req.body });
      next();
    });

    // Error handling
    this.app.use((err, req, res, next) => {
      logger.error('Request error', { error: err.message, stack: err.stack });
      res.status(500).json({
        success: false,
        error: err.message
      });
    });
  }

  setupRoutes() {
    // Health check
    this.app.get('/api/health', async (req, res) => {
      try {
        const health = await this.agent.healthCheck();
        res.json(health);
      } catch (error) {
        res.status(503).json({
          status: 'unhealthy',
          error: error.message
        });
      }
    });

    // Pull repository (synchronous - returns immediately with result)
    this.app.post('/api/pull', async (req, res) => {
      try {
        const { url, branch, username, password } = req.body;

        if (!url) {
          return res.status(400).json({
            success: false,
            error: 'Missing required parameter: url'
          });
        }

        const result = await this.agent.pull(url, branch, username, password);
        res.json(result);

      } catch (error) {
        logger.error('Pull failed', { error: error.message });
        res.status(500).json({
          success: false,
          error: error.message
        });
      }
    });
  }

  start() {
    const port = this.agentConfig.port || 3000;

    this.server = this.app.listen(port, () => {
      logger.info(`ABAP AI Bridge server started on port ${port}`);
      console.log(`\n🚀 ABAP AI Bridge is running!`);
      console.log(`   Health:   http://localhost:${port}/api/health`);
      console.log(`   Pull:     POST http://localhost:${port}/api/pull`);
      console.log(`\n📚 API Documentation:`);
      console.log(`   POST /api/pull { "url": "git-url", "branch": "main" }`);
    });

    // Graceful shutdown
    process.on('SIGTERM', () => this.shutdown());
    process.on('SIGINT', () => this.shutdown());
  }

  shutdown() {
    logger.info('Shutting down server...');
    if (this.server) {
      this.server.close(() => {
        logger.info('Server closed');
        process.exit(0);
      });
    }
  }
}

// Start server if run directly
if (require.main === module) {
  const server = new Server();
  server.start();
}

module.exports = {
  Server
};
