UPDATE action SET 
  name = ?, 
  description = ? 
WHERE
  id = ?;
UPDATE action_bar SET 
   
WHERE
  ship_id = ? AND action_id = ? AND position = ?;
UPDATE firing_arc SET 
  name = ?, 
  primary_attack = ?, 
  description = ? 
WHERE
  id = ?;
UPDATE huge_maneuver_dial SET 
  energy = ? 
WHERE
  ship_id = ? AND speed = ? AND maneuver_id = ?;
UPDATE huge_ship SET 
  energy = ?, 
  min_primary_range = ?, 
  max_primary_range = ? 
WHERE
  ship_id = ?;
UPDATE maneuver SET 
  name = ?, 
  description = ? 
WHERE
  id = ?;
UPDATE maneuver_dial SET 
  difficulty = ? 
WHERE
  ship_id = ? AND speed = ? AND maneuver_id = ?;
UPDATE pilot SET 
  name = ?, 
  variant = ?, 
  u_name_id = ?, 
  ship_id = ?, 
  pilot_skill = ?, 
  elite = ?, 
  point_cost = ?, 
  text_box = ? 
WHERE
  id = ?;
UPDATE ship SET 
  name = ?, 
  variant = ?, 
  faction = ?, 
  `size` = ?, 
  attack = ?, 
  agility = ?, 
  hull = ?, 
  shield = ? 
WHERE
  id = ?;
UPDATE ship_firing_arc SET 
   
WHERE
  ship_id = ? AND firing_arc_id = ?;
UPDATE unique_name SET 
  name = ? 
WHERE
  id = ?;
UPDATE upgrade SET 
  name = ?, 
  description = ? 
WHERE
  id = ?;
UPDATE upgrade_bar SET 
   
WHERE
  ship_id = ? AND upgrade_id = ? AND position = ?;

