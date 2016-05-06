> module Jerome where
> import List
> import Maybe
>
>
> data Sentence = Simple Subject Verb Object
>               | Conjuncted Sentence Conjunction Sentence
>               | Objectless Subject Verb
>                      deriving Eq
>
> data Conjunction = And | But | AndThen   deriving Eq
>      -- sorry, very english those conjunctions
>      -- really should be using men, de, kai, etc.
>
> data Subject = NoSubject | SimpleSubject Noun | CompoundSubject [Noun]
>                    deriving (Eq,Show)
> data Object = NoObject | SimpleObject Noun | CompoundObject [Noun]
>                    deriving (Eq,Show)
> data Verb = SimpleVerb Tense Voice VerbID
>                    deriving (Eq,Show)
> type VerbID = Integer
> data Tense = Present | Emphatic | Continuous | Perfect | PluPerfect 
>                 | Aorist | Infinitive | Imperative
>                    deriving (Eq,Enum,Show)
> data Voice = Passive | Active | Middle 
>                    deriving (Eq,Enum,Show)
> data Noun = Cedent Pronoun | Definitive Number NaturalGender NounID
>                | Indefinite Number NaturalGender NounID
>                    deriving (Eq,Show)
> type NounID = Integer
> data Pronoun = FirstPersonInclusive Number NaturalGender
>             | FirstPersonExclusive Number NaturalGender
>             | SecondPerson Number NaturalGender
>             | ThirdPerson Number NaturalGender
>                    deriving (Eq,Show)

Maybe I'm being too restrictive here. Can first person duals exist?
What about second or thirds?

> data Person = NoPerson | FirstPersonSingular | SecondPersonSingular |
>    ThirdPersonSingular | FirstPersonPluralInclusive | 
>    FirstPersonPluralExclusive | SecondPersonPlural |
>    ThirdPersonPlural
> data Number = Singular | Plural | Dual | SmallPlural
>                    deriving (Eq,Enum,Show)
> data NaturalGender = Masculine | Feminine | Neuter
>                        deriving (Eq,Enum,Show)
>
>

Need to introduce a new type here to handle these...
I wonder whether I've covered every base here - "to have" has an irregular
present but a very simple past.

> data EnglishVerbForm = 
>         RegularVerb String 
>       | IrregularPerfect String String -- infinitive, perfect
>       | VeryIrregularVerb String [String] [String] -- infinite, present, perfect
>
>
> data EnglishNounForm =
>         RegularNoun String
>       | NonPluralNoun String -- e.g. fish, sheep
>       | VeryIrregularNoun String String -- e.g. child-children

Back to generic stuff. 

>
> person_of_pronoun :: Pronoun -> Person
> person_of_pronoun (FirstPersonInclusive Singular _) = FirstPersonSingular
> person_of_pronoun (FirstPersonInclusive Plural _) = FirstPersonPluralInclusive
> person_of_pronoun (FirstPersonExclusive Singular _) = error "Singular exclusive?"
> person_of_pronoun (FirstPersonExclusive Plural _) = FirstPersonPluralExclusive
> person_of_pronoun (SecondPerson Singular _) = SecondPersonSingular
> person_of_pronoun (SecondPerson Plural _) = SecondPersonPlural
> person_of_pronoun (ThirdPerson Singular _) = ThirdPersonSingular
> person_of_pronoun (ThirdPerson Plural _) = ThirdPersonPlural
> person_of_subject NoSubject = NoPerson
> person_of_subject (CompoundSubject x) 
>  | length x > 1 = ThirdPersonPlural  -- what about duals, small plurals?
>  | x == [] = NoPerson
>  | otherwise = person_of_subject (SimpleSubject (head x))
> person_of_subject (SimpleSubject (Cedent pronoun)) = person_of_pronoun pronoun
> person_of_subject (SimpleSubject (Definitive number _ _)) 
>   | number == Singular = ThirdPersonSingular
>   | number == Plural = ThirdPersonPlural
>   | otherwise = error "Oops, odd situation does occur 1358"    
> person_of_subject (SimpleSubject (Indefinite number _ _)) 
>   | number == Singular = ThirdPersonSingular
>   | number == Plural = ThirdPersonPlural
>   | otherwise = error "Oops, odd situation does occur 1359"    
> 
> he = Cedent (ThirdPerson Singular Masculine)
> to_open = 10001
> opened = SimpleVerb Perfect Active to_open
>  -- skipping "his", replacing with "a"
> mouth = 10002
> mouth' = Definitive Singular Neuter mouth
> to_teach = 10003
> taught = SimpleVerb Perfect Active to_teach
> them = Cedent (ThirdPerson  Plural Neuter)
> crowd = 10008
> the_crowds = Definitive Plural Neuter crowd
>
> to_be = 10004
> to_go = 10005
> to_have = 10006
> to_see = 10007
> went = SimpleVerb Perfect Active to_go
> english_noun :: Number -> Integer -> [String]
> english_noun number x = english_noun' number (lookup x english_nouns)
> english_noun' :: Number -> EnglishNounForm -> [String]
> english_noun' Singular (RegularNoun form) = [form]
> english_noun' Plural (RegularNoun form) = [english_add_s form]
> english_noun' Singular (NonPluralNoun form) = [form]
> english_noun' Plural (NonPluralNoun form) = [form]
> english_noun' Singular (VeryIrregularNoun _ form) = [form]
> english_noun' Plural (VeryIrregularNoun form _) = [form]

> english_noun Singular 10002 = ["mouth"]
> english_noun Plural 10002 = ["mouths"]
> mouth = Definitive Singular Neuter x_mouth
>
> english_nouns = 
>   [ (crowd, RegularNoun "crowd"),
>     (mouth, RegularNoun "mouth")
>   ]
> english_verbs =
>   [ (to_open,  RegularVerb "open"),
>     (to_teach, IrregularPerfect "teach" "taught"),
>     (to_be,    VeryIrregularVerb "be" 
>                   ["am", "are", "is", "are", "are", "are" ]
>                   ["was", "were", "was", "were", "were", "were"]),
>     (to_go,    IrregularPerfect "go" "went"),
>     (to_have,  VeryIrregularVerb "have"
>                   ["have","have","has","have","have","have"]
>                   ["had","had","had", "had","had","had"])
>     (to_see,   IrregularPerfect "see" "saw"]
>   ]

5:1 
 Now when he saw the crowds, he went up on a mountainside and sat down.
 His disciples came to him, and
Matthew 5:2 is really "Then he opened his mouth and he taught them, saying..."
But we are going to pretend that it is "He opened his mouth. He taught them"

> matthew_5 =
>  [
>   (Simple (SimpleSubject he) saw (SimpleObject the_crowds),
>   Conjuncted 
>      (Objectless (SimpleSubject he) went)
>      (Objectless (SimpleSubject) sat_down)
>   ,
>   (Objectless (SimpleSubject the_disciples) came
>   ,
>   Conjuncted
>    (Simple (SimpleSubject he) opened (SimpleObject mouth'))
>    And
>    (Simple (SimpleSubject he) taught (SimpleObject them))
>   ]
>
>
>
> english_verb_id_to_form :: Integer -> EnglishVerbForm
> english_verb_id_to_form id = fromJust (lookup id english_verbs)
>
> english_present :: Person -> EnglishVerbForm -> [String]
> english_present ThirdPersonSingular (RegularVerb form) =
>   [english_add_s form]
> english_present _ (RegularVerb form) = [form]
> english_present ThirdPersonSingular (IrregularPerfect form _) =
>   [english_add_s form]
> english_present _ (IrregularPerfect form _) = [form]
> english_present person (VeryIrregularVerb _ presents _)
>  = [presents !! (verbalFormPosition person)]
>
> english_perfect :: Person -> EnglishVerbForm -> [String]
> english_perfect _ (RegularVerb text) = [english_add_ed text]
> english_perfect _ (IrregularPerfect _ text) = [text]
> english_perfect person (VeryIrregularVerb _ _ perfects) =
>   [perfects !! (verbalFormPosition person)]
> 
> verbalFormPosition FirstPersonSingular =  0
> verbalFormPosition SecondPersonSingular = 1
> verbalFormPosition ThirdPersonSingular =  2
> verbalFormPosition FirstPersonPluralInclusive =  3
> verbalFormPosition FirstPersonPluralExclusive =  3
> verbalFormPosition SecondPersonPlural =  4
> verbalFormPosition ThirdPersonPlural =  5
> 
> 
> english_add_s word 
>  | any (\x -> isSuffixOf x word) endings_needing_e = word ++ "es"
>  | isSuffixOf "y" word = ((reverse.tail.reverse) word) ++ "ies"
>  | otherwise = word ++ "s"
>  where endings_needing_e = ["sh","ch","th","gh"]
>
> english_add_ed word
>  | isSuffixOf "e" word = word ++ "d"
>  | isSuffixOf "y" word = ((reverse.tail.reverse) word) ++ "ied"
>  | otherwise = word ++ "ed"

 data Person = NoPerson | FirstPersonSingular | SecondPersonSingular |
    ThirdPersonSingular | FirstPersonPluralInclusive | 
    FirstPersonPluralExclusive | SecondPersonPlural |
    ThirdPersonPlural


> capitalise_word (letter : letters) = (toUpper letter) : letters
> english_punctuate :: [String] -> String
> english_punctuate (word:words) = 
>    english_spacify ((capitalise_word word) : words) ++ ". "
> english_spacify :: [String] -> String
> english_spacify words = concat (intersperse " " words)
> english_conjunct :: [String] -> [String]
> english_conjunct [word] = [word]
> english_conjunct ([word1,word2]) = [word1,"and",word2]
> english_conjunct (words) = reverse (last_word:"and":earlier_words')
>   where words' = reverse words
>         (last_word:earlier_words) = words'
>         earlier_words' = map english_add_trailing_comma earlier_words
> english_add_trailing_comma word = word ++ ","
> english_trail_comma words =
>    reverse (((head rev_words) ++ ",") : (tail rev_words))
>  where rev_words = reverse words
> english_sentence :: Sentence -> String
> english_sentence x = english_punctuate (english_sentence' x)
> english_sentence' (Simple subject verb object) = 
>         english_subject subject  ++ 
>         english_verb subject verb  ++
>         english_object object
> english_sentence' (Conjuncted sentence1 conjunction sentence2)
>  = english_trail_comma (english_sentence' sentence1)
>        ++ english_conjunction conjunction ++ english_sentence' sentence2
> english_conjunction And = ["and"]
> english_conjunction But = ["but"]
> english_conjunction AndThen = ["and","then"]

> english_indefinite_article :: [String] -> [String]
> english_indefinite_article next_words 
>   | first_letter == 'a'  
>     || first_letter == 'e' || first_letter == 'i'  
>     || first_letter == 'o' || first_letter == 'u'
>      -- or first_letter == 'h' -- but it sounds odd to me; 
>      -- I wonder if there are any other such cases?
>     = "an":next_words
>   | otherwise = "a":next_words
>  where next_word = head next_words
>        first_letter = head next_word
>
> english_subject_pronoun (SecondPerson _ _) = ["you"]
> english_subject_pronoun (ThirdPerson Singular Masculine) = ["he"]
> english_subject_pronoun (ThirdPerson Singular Feminine) = ["she"]
> english_subject_pronoun (ThirdPerson Singular Neuter) = ["it"]
> english_subject_pronoun (ThirdPerson _ _) = ["they"]
> english_subject_pronoun (FirstPersonInclusive Singular _ ) = ["I"]
> english_subject_pronoun (FirstPersonExclusive Singular _ ) = ["I"]
> english_subject_pronoun _ = ["we"]

> english_subject :: Subject -> [String]
> english_subject NoSubject = []
> english_subject (SimpleSubject (Cedent pronoun)) = 
>     english_subject_pronoun pronoun
> english_subject (SimpleSubject (Definitive Singular _ nounId)) =         
>                                 "the":(english_noun Singular nounId)
> english_subject (SimpleSubject (Definitive _ _ nounId)) = 
>                                 "the":(english_noun Plural nounId) 
> english_subject (SimpleSubject (Indefinite Singular _ nounId)) = 
>       english_indefinite_article (english_noun Singular nounId)
> english_subject (SimpleSubject (Indefinite number _ nounId)) = 
>      "the":(english_noun number nounId)
> english_subject (CompoundSubject nouns) = 
>    english_conjunct (concat
>         (map (\x -> english_subject (SimpleSubject x)) nouns)
>     )
   

 data Verb = SimpleVerb Tense Voice VerbID
                    deriving (Eq,Show)
 type VerbID = Integer
 data Tense = Present | Emphatic | Continuous | Perfect | PluPerfect 
                 | Aorist | Infinitive | Imperative
                    deriving (Eq,Enum,Show)
 data Voice = Passive | Active | Middle 
                    deriving (Eq,Enum,Show)
                  

I think that there are no human languages in which the verb has to
agree with the object. (Lots of Indo-European languages have verb
forms that change with the subject.) But since I am coding each 
language anyway, that's not too big a deal.

> english_verb :: Subject -> Verb -> [String]
> english_verb subject (SimpleVerb tense Middle verbid) = 
>   error "can't handle middle voice yet, if ever"
> english_verb subject (SimpleVerb Present Active verbid) =
>   english_present (person_of_subject subject) 
>                     (english_verb_id_to_form verbid)
> english_verb subject (SimpleVerb Perfect Active verbid) =
>   english_perfect (person_of_subject subject)
>                     (english_verb_id_to_form verbid)
> english_verb _ (SimpleVerb _ _ _) = error "Unimplemented form"
>
> english_object_pronoun (SecondPerson _ _) = ["you"]
> english_object_pronoun (ThirdPerson Singular Masculine) = ["him"]
> english_object_pronoun (ThirdPerson Singular Feminine) = ["her"]
> english_object_pronoun (ThirdPerson Singular Neuter) = ["it"]
> english_object_pronoun (ThirdPerson _ _) = ["them"]
> english_object_pronoun (FirstPersonInclusive Singular _ ) = ["me"]
> english_object_pronoun (FirstPersonExclusive Singular _ ) = ["me"]
> english_object_pronoun _ = ["us"]
>
> english_object :: Object -> [String]
> english_object NoObject = []
> english_object (SimpleObject (Cedent pronoun)) = 
>     english_object_pronoun pronoun
> english_object (SimpleObject (Definitive Singular _ nounId)) =         
>                                 "the":(english_noun Singular nounId)
> english_object (SimpleObject (Definitive _ _ nounId)) = 
>                                 "the":(english_noun Plural nounId) 
> english_object (SimpleObject (Indefinite Singular _ nounId)) = 
>       english_indefinite_article (english_noun Singular nounId)
> english_object (SimpleObject (Indefinite number _ nounId)) = 
>      "the":(english_noun number nounId)
> english_object (CompoundObject nouns) = 
>    english_conjunct (concat
>         (map (\x -> english_object (SimpleObject x)) nouns)
>     )
>   
>
> -- main = putStr "Welcome to Greg's translator\n"
> main = putStr ((concat passage) ++ "\n")
>  where passage = map english_sentence matthew_5_2

Significant dates:
 
  5-Oct-2003: Started. Went to church early and had nothing better to do.
 17-Oct_2003: First verse translated into simplified English - Matthew 5:2
              Matthew 5:2 is really "Then he opened his mouth and he taught
              them, saying..."  But we are going to pretend that it is "He
              opened his mouth. He taught them"
