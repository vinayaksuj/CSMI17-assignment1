% 'none' indicates no siblings
siblings(none). 

% Rules
% The speaker's father is X, and "my father's son" must be the speaker if no siblings exist
father(X, speaker).               % X is the father of the speaker
father(speaker, ThatMan).         % Speaker is the father of "that man"

% Query to find who "that man" is
?- father(speaker, ThatMan).
