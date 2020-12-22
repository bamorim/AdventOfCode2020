namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared
open System.Text.RegularExpressions

module Day21 =
    type Ingredient = string
    type Allergen = string
    type Food = Set<Ingredient> * Set<Allergen>
    type Input = seq<Food>

    let parseFood (line: string): Food =
        let m =
            Regex.Match(line, "^(?<ingredients>(\w+)( \w+)*) \(contains (?<allergens>(\w+)(, \w+)*)\)$")

        if m.Success then
            let ingredients =
                m.Groups.["ingredients"].Value.Split(" ")
                |> Set.ofSeq

            let allergens =
                m.Groups.["allergens"].Value.Split(", ")
                |> Set.ofSeq

            ingredients, allergens
        else
            failwith "Invalid food format"

    let parseFile: string -> seq<Food> = IO.File.ReadLines >> Seq.map parseFood

    let containAllergen ((_, allergens): Food) (allergen: Allergen): Boolean = Set.contains allergen allergens

    let containIngredient ((ingredients, _): Food) (ingredient: Ingredient): Boolean =
        Set.contains ingredient ingredients

    let foodBy selector foods =
        seq {
            for food in foods do
                for key in selector food do
                    yield (key, food)
        }
        |> Seq.groupBy fst
        |> Seq.map (fun (allergen, foods) -> (allergen, Seq.map snd foods))
        |> Map.ofSeq

    let getIngredientsByPossibleAllergens (foods: seq<Food>): Map<Ingredient, Set<Allergen>> =
        // Note: there might be missing some food since now all allergens are listed for all food
        let foodByAllergen = foodBy snd foods

        let allAllergens =
            foods |> Seq.map snd |> Seq.reduce Set.union

        let allIngredients =
            foods |> Seq.map fst |> Seq.reduce Set.union

        allIngredients
        |> Seq.map (fun ingredient ->
            let allergens =
                Set.filter (fun allergen ->
                    foodByAllergen
                    |> Map.find allergen
                    |> Seq.forall (fun (ingredients, _) -> Set.contains ingredient ingredients)) allAllergens

            (ingredient, allergens))
        |> Map.ofSeq

    let getInertIngredients (foods: seq<Food>): Set<Ingredient> =
        foods
        |> getIngredientsByPossibleAllergens
        |> Map.toSeq
        |> Seq.filter (fun (ingredient, allergens) -> Set.isEmpty allergens)
        |> Seq.map fst
        |> Set.ofSeq


    let part1 (foods: seq<Food>): int =
        let foodByIngredient = foodBy fst foods

        foods
        |> getInertIngredients
        |> Seq.sumBy (fun ingredient ->
            foodByIngredient
            |> Map.find ingredient
            |> Seq.length)

    let getAllergenIngredients (foods: seq<Food>): Map<Allergen, Ingredient> =
        let inertIngredients = getInertIngredients foods

        let removeInertIngredients ((ingredients, allergens): Food): Food =
            (Set.difference ingredients inertIngredients, allergens)

        let rec aux result (remaining: seq<Ingredient * Set<Allergen>>): Map<Allergen, Ingredient> =
            let exactMatches =
                seq {
                    for (ingredient, allergens) in remaining do
                        if Set.count allergens = 1 then yield (Seq.head allergens, ingredient)
                }

            if Seq.isEmpty exactMatches then
                result
            else
                let newResult =
                    Seq.fold (fun result (allergen, ingredient) -> Map.add allergen ingredient result) result
                        exactMatches

                let newRemaining =
                    remaining
                    |> Seq.map (fun (ingredient, allergens) ->
                        let filteredAllergens =
                            Set.filter (fun allergen -> not <| Map.containsKey allergen newResult) allergens

                        (ingredient, filteredAllergens))
                    |> Seq.filter (snd >> Set.isEmpty >> not)

                aux newResult newRemaining

        foods
        |> getIngredientsByPossibleAllergens
        |> Map.filter (fun _ -> Set.isEmpty >> not)
        |> Map.toSeq
        |> aux Map.empty

    let part2 (foods: seq<Food>): string =
        foods
        |> getAllergenIngredients
        |> Map.toSeq
        |> Seq.sortBy fst
        |> Seq.map snd
        |> Array.ofSeq
        |> (fun ingredients -> String.Join(',', ingredients))

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
