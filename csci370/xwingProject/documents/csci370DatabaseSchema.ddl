CREATE TABLE firing_arc (
  id             TINYINT(2)               NOT NULL,
  name           VARCHAR(50)              NOT NULL,
  primary_attack BOOL                     NOT NULL,
  description    VARCHAR(1000) DEFAULT '' NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE action (
  id          TINYINT(2)    NOT NULL,
  name        VARCHAR(50)   NOT NULL,
  description VARCHAR(1000) NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE maneuver (
  id          TINYINT(2)               NOT NULL,
  name        VARCHAR(50)              NOT NULL,
  description VARCHAR(1000) DEFAULT '' NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE upgrade (
  id          TINYINT(2)               NOT NULL,
  name        VARCHAR(50)              NOT NULL,
  description VARCHAR(1000) DEFAULT '' NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE unique_name (
  id   SMALLINT(3) NOT NULL AUTO_INCREMENT,
  name VARCHAR(50) NOT NULL UNIQUE,
  PRIMARY KEY (id)
);

CREATE TABLE ship (
  id      TINYINT(2)                         NOT NULL,
  name    VARCHAR(50)                        NOT NULL,
  variant VARCHAR(20) DEFAULT ''             NOT NULL,
  faction ENUM ('Rebel', 'Imperial', 'Scum') NOT NULL,
  `size`  ENUM ('Small', 'Large', 'Huge')    NOT NULL,
  attack  TINYINT(1),
  agility TINYINT(1),
  hull    TINYINT(1),
  shield  TINYINT(1),
  PRIMARY KEY (id),
  CONSTRAINT uq_ship UNIQUE (name, variant)
);

CREATE TABLE huge_ship (
  ship_id           TINYINT(2) NOT NULL,
  energy            TINYINT(1),
  min_primary_range TINYINT(1),
  max_primary_range TINYINT(1),
  PRIMARY KEY (ship_id),
  FOREIGN KEY FK_huge_ship_01 (ship_id) REFERENCES ship (id)
);

CREATE TABLE pilot (
  id          SMALLINT(3)            NOT NULL AUTO_INCREMENT,
  name        VARCHAR(50)            NOT NULL,
  variant     VARCHAR(20) DEFAULT '' NOT NULL,
  u_name_id   SMALLINT(3)            NOT NULL,
  ship_id     TINYINT(2)             NOT NULL,
  pilot_skill TINYINT(1)             NOT NULL,
  elite       BOOL,
  point_cost  TINYINT(2),
  text_box    VARCHAR(100)                    DEFAULT '',
  PRIMARY KEY (id),
  CONSTRAINT uq_pilot UNIQUE (name, variant),
  FOREIGN KEY FK_pilot_01 (u_name_id) REFERENCES unique_name (id),
  FOREIGN KEY FK_pilot_02 (ship_id) REFERENCES ship (id)
);

CREATE TABLE maneuver_dial (
  ship_id     TINYINT(2)           NOT NULL,
  speed       TINYINT(1)           NOT NULL,
  maneuver_id TINYINT(2)           NOT NULL,
  difficulty  ENUM ('G', 'W', 'R') NOT NULL,
  PRIMARY KEY (ship_id, speed, maneuver_id),
  FOREIGN KEY FK_maneuver_dial_01 (maneuver_id) REFERENCES maneuver (id),
  FOREIGN KEY FK_maneuver_dial_02 (ship_id) REFERENCES ship (id)
);

CREATE TABLE huge_maneuver_dial (
  ship_id     TINYINT(2) NOT NULL,
  speed       TINYINT(1) NOT NULL,
  maneuver_id TINYINT(2) NOT NULL,
  energy      TINYINT(1) NOT NULL,
  PRIMARY KEY (ship_id, speed, maneuver_id),
  FOREIGN KEY FK_huge_maneuver_01 (maneuver_id) REFERENCES maneuver (id),
  FOREIGN KEY FK_huge_maneuver_02 (ship_id) REFERENCES huge_ship (ship_id)
);

CREATE TABLE upgrade_bar (
  ship_id    TINYINT(2) NOT NULL,
  upgrade_id TINYINT(2) NOT NULL,
  position   TINYINT(1) NOT NULL,
  PRIMARY KEY (ship_id, upgrade_id, position),
  FOREIGN KEY FK_upgrade_bar_01 (upgrade_id) REFERENCES upgrade (id),
  FOREIGN KEY FK_upgrade_bar_02 (ship_id) REFERENCES ship (id)
);

CREATE TABLE action_bar (
  ship_id   TINYINT(2) NOT NULL,
  action_id TINYINT(2) NOT NULL,
  position  TINYINT(1) NOT NULL,
  PRIMARY KEY (ship_id, action_id, position),
  FOREIGN KEY FK_action_bar_01 (ship_id) REFERENCES ship (id),
  FOREIGN KEY FK_action_bar_02 (action_id) REFERENCES action (id)
);

CREATE TABLE ship_firing_arc (
  ship_id       TINYINT(2) NOT NULL,
  firing_arc_id TINYINT(2) NOT NULL,
  PRIMARY KEY (ship_id, firing_arc_id),
  FOREIGN KEY FK_ship_firing_arc_01 (ship_id) REFERENCES ship (id),
  FOREIGN KEY FK_ship_firing_arc_02 (firing_arc_id) REFERENCES firing_arc (id)
);


