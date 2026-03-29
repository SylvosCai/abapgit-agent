'use strict';

/**
 * abapgit-agent command registry
 *
 * Each entry maps a CLI command name to its command module.
 * Command modules must export: { name, description, requiresAbapConfig, execute }
 */
module.exports = {
  help:      require('./help'),
  health:    require('./health'),
  status:    require('./status'),
  create:    require('./create'),
  delete:    require('./delete'),
  import:    require('./import'),
  inspect:   require('./inspect'),
  unit:      require('./unit'),
  syntax:    require('./syntax'),
  tree:      require('./tree'),
  list:      require('./list'),
  view:      require('./view'),
  preview:   require('./preview'),
  where:     require('./where'),
  dump:      require('./dump'),
  debug:     require('./debug'),
  run:       require('./run'),
  ref:       require('./ref'),
  guide:     require('./guide'),
  init:      require('./init'),
  pull:      require('./pull'),
  upgrade:   require('./upgrade'),
  transport: require('./transport'),
  lint:      require('./lint')
};
