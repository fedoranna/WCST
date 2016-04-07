%% Read me

% GENERAL
% This script generates all the possible cards and their possible target
% positions according to four simple (one-dimensional) rules in the
% Wisconsin Card Sorting Task. It also generates semi-randomly composed
% decks of cards (in the order specified by the index rule) that can be 
% directly used in the WCST.

% The reference cards are the following:
%   At position A: one red triangle
%   At position B: two green stars
%   At position C: three yellow squares
%   At position D: four blue circles

% PARAMETERS
% In the parameters section you can choose whether to use ambiguous cards
% and you can choose the sequence for the index rule. 
% An unambiguous card is a card that would be placed to different target
% positions according to each rule, i.e., from the target position we can
% unambiguously tell which simple (one-dimensional) rule is consistent with
% the move. 
% The sequence can be of any length, but should only contain numbers 
% 1,2,3,4. If the sequence is longer than 4 items, the humantable will not 
% be generated.

% OUTPUTS
% The script will print two tables in the command window. In these tables,
% each row represents a card. The cards are ordered according to the
% sequence so the tables can be used as the deck of cards.

% 1. "lookup" (matrix)
% This is a table with 8 columns, where columns 1-4 code the features of
% cards and column 5-8 code the target positions of the cards
% Column 1 (number): the number of figures on the card
%   1=one, 2=two, 3=three, 4=four
% Column 2 (color): the color of the figures on the card
%   1=red, 2=green, 3=yellow, 4=blue
% Column 3 (shape): the shape of the figures on the card
%   1=triangle, 2=star, 3=square, 4=circle
% Column 4 (index): the index of the card in the ordered sequence
%   1=first, 2=second, 3=third, 4=fourth
% Column 5 (target according to number rule): 1=A, 2=B, 3=C, 4=D
% Column 6 (target according to color rule): 1=A, 2=B, 3=C, 4=D
% Column 7 (target according to shape rule): 1=A, 2=B, 3=C, 4=D
% Column 8 (target according to index rule): 1=A, 2=B, 3=C, 4=D

% 2. "humantable" (cell)
% This includes the same information as "lookup", but in words

% 3. "decks" (matrix)
% Semi-randomly composed decks of cards

% 4. lookup_t (matrix; not printed)
% This includes the same information as "lookup" but translated for binary
% code

% GENERATING THE DECK OF CARDS
% This table is the same as "lookup" but the rows are reordered, so that it
% can be used in this order 

%% Parameters

ambiguous = 0;          % Do you want to use ambiguous cards? 0=no, 1=yes
sequence = [1,2,3,4];   % The sequence of target positions according to the index rule; 1=A, 2=B, 3=C, 4=D. If this is left empty, the s-th sequence will be chosen from all possible sequences
s = 1;                  % 1 to 16; the index of the sequence to choose from all possible sequences of length 4
nbof_decks = 2;         % How many decks would you like to randomly compose?
seed = 1;               % random seed

%% Possible sequences for the 4th rule

rng(seed);

if isempty(sequence)
    % All possible four-item-long sequences (where all positions occur exactly once)
    sequences = perms(1:4);
    
    % Delete sequences where cards are placed sequentially from left to right or from right to left more than once
    directions = NaN(size(sequences));
    directions(:,1) = sequences(:,1)-sequences(:,4);
    directions(:,2) = sequences(:,2)-sequences(:,1);
    directions(:,3) = sequences(:,3)-sequences(:,2);
    directions(:,4) = sequences(:,4)-sequences(:,3);
    directions = abs(directions);
    directions = directions == 1;
    for i = size(sequences,1):-1:1
        if sum(directions(i,:)) > 2
            sequences(i,:)=[];
        end
    end
    
    % Chose one sequence
    sequence = sequences(s,:);
end

% Calculate sequential dependencies based on the order of cards
dependencyrule = NaN(size(sequence));
for j = 1:numel(sequence)-1
    dependencyrule(sequence(j)) = sequence(j+1);
end
dependencyrule(sequence(end)) = sequence(1);

%% Compose lookup table with all possible target positions


if ambiguous == 1
    lookup = NaN(4^4, 8); % with ambiguous cards (256): sometimes more then one rule predicts the same position
    lookup(:,8) = repmat([repmat(1,4^0,1);repmat(2,4^0,1);repmat(3,4^0,1);repmat(4,4^0,1)], 4^3,1);
    lookup(:,7) = repmat([repmat(1,4^1,1);repmat(2,4^1,1);repmat(3,4^1,1);repmat(4,4^1,1)], 4^2,1);
    lookup(:,6) = repmat([repmat(1,4^2,1);repmat(2,4^2,1);repmat(3,4^2,1);repmat(4,4^2,1)], 4^1,1);
    lookup(:,5) = repmat([repmat(1,4^3,1);repmat(2,4^3,1);repmat(3,4^3,1);repmat(4,4^3,1)], 4^0,1);
end
if ambiguous == 0 % without ambiguous cards (24): each rule puts the card in a different target position
    lookup = NaN(factorial(4),8);
    permutations = perms(1:4);
    lookup(:,5:7) = permutations(:,1:3);
    lookup(:,8) = permutations(:,4);
    lookup = sortrows(lookup);
end

%% Apply rules

% Index rule
for i = size(lookup,1):-1:1 % fill in the index based on the target position
    if sum(sequence==lookup(i,8)) == 0 % for positions that never occur in the sequence
        lookup(i,:) = [];
    elseif sum(sequence==lookup(i,8)) > 1 % for positions that occur more than once in the sequence
        % leave it as NaN
    else % for positions that occur once in the sequence
        lookup(i,4) = find(sequence==lookup(i,8));
    end
end

% For positions that occur more than once in the sequence, we sort the indexes as equally as possible
ambitargets = [];
for i = 1:size(lookup,1)
    if isnan(lookup(i,4))
        ambitargets = [ambitargets, lookup(i,8)];
    end
end
u_ambitargets = unique(ambitargets);
for u = 1:numel(u_ambitargets)
    a = u_ambitargets(u);
    poss_indexes = find(sequence==a);
    nbof_possindexes = numel(poss_indexes);
    next=1;
    for i = 1:size(lookup,1)
        if lookup(i,8) == a
            lookup(i,4) = poss_indexes(next);
            next = next+1;
            if next > nbof_possindexes
                next = 1;
            end
        end
    end
end

% Basic rules
lookup(:,1:3) = lookup(:,5:7);

%% Compose the deck of cards from lookup
% Each deck is composed of all the cards, but some cards might be present
% several times. This is sometimes necessary to be able to use all cards in 
% the sequences. The duplication of cards is not random, but in order. The
% composition of the deck is random. "decks" can include more than one
% deck, and the order of cards will be different in each deck. 

% Duplicate cards if there is not enought from some if the indexes
nbof_indexes = NaN(1,numel(sequence));
indexof_indexes = cell(1,numel(sequence));
for i = 1:numel(sequence)
    nbof_indexes(i) = numel(find(lookup(:,4) == i));
    indexof_indexes{i} = find(lookup(:,4) == i);
end
for i = 1:numel(indexof_indexes)
    if numel(indexof_indexes{i}) < max(nbof_indexes)
        repeats = max(nbof_indexes) - numel(indexof_indexes{i});
        next = 1;
        for j=1:repeats
            indexof_indexes{i} = [indexof_indexes{i}; indexof_indexes{i}(next)];
            if next < numel(indexof_indexes{i})
                next=next+1;
            else
                next = 1;
            end
        end
    end
end

% Randomly compose decks
decks = NaN(0,8);
for i = 1:nbof_decks
    deck = NaN(max(nbof_indexes)*numel(sequence),8);
    for j = 1:numel(indexof_indexes)
        p = randperm(max(nbof_indexes));
        indexof_indexes{j} = indexof_indexes{j}(p);
    end
    next = 1;
    for j = 1:max(nbof_indexes)
        for k = 1:numel(sequence)
            deck(next,:) = lookup(indexof_indexes{k}(j),:);
            next = next+1;
        end
    end
    decks = [decks; deck];
end

%% Translate lookup table for binary neurons
% 
% lookup_t = lookup';
% lookup_bin = zeros(4, numel(lookup));
% 
% for i = 1:numel(lookup_t)
%     lookup_bin(lookup_t(i),i)=1;
% end
% activation = [lookup_bin(:)]';

%% Translate lookup table for humans

words = cell(4,5);
words{1,1}='one';       % Number
words{2,1}='two';
words{3,1}='three';
words{4,1}='four';
words{1,2}='red';       % Colour
words{2,2}='green';
words{3,2}='yellow';
words{4,2}='blue';
words{1,3}='triangle';	% Shape
words{2,3}='star';
words{3,3}='rectangle';
words{4,3}='circle';
words{1,4}='first';     % Index
words{2,4}='second';
words{3,4}='third';
words{4,4}='fourth';
words{1,5}='A';         % Target position
words{2,5}='B';
words{3,5}='C';
words{4,5}='D';
words{1,6}='A';         % Target position
words{2,6}='B';
words{3,6}='C';
words{4,6}='D';
words{1,7}='A';         % Target position
words{2,7}='B';
words{3,7}='C';
words{4,7}='D';
words{1,8}='A';         % Target position
words{2,8}='B';
words{3,8}='C';
words{4,8}='D';

humantable = cell(size(lookup));
for i=1:size(lookup,1)
    for j=1:size(lookup,2)
        humantable{i,j} = words{lookup(i,j),j};
    end
end

%% Print

lookup
humantable
decks






























