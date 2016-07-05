package com.gvolpe.demo.copy

import shapeless._

// Taken from: https://gist.github.com/Baccata/b45a4a5eea1805404bf9

object copySyntax {

  implicit class CopySyntax[TypeToCopy](thingToCopy: TypeToCopy) {

    object copy extends RecordArgs {
      def applyRecord[RecordOfFieldsAndValues <: HList]
      (newFieldsAndValue: RecordOfFieldsAndValues)
      (implicit update: UpdaterFromRecord[TypeToCopy, RecordOfFieldsAndValues]): TypeToCopy = update(thingToCopy, newFieldsAndValue)
    }

  }

}

trait UpdaterFromRecord[TypeToCopy, RecordOfFieldsAndValues <: HList] {
  def apply(thingToCopy: TypeToCopy, newFieldsAndValues: RecordOfFieldsAndValues): TypeToCopy
}

object UpdaterFromRecord {

  import ops.record._

  implicit def generateUpdaterFromMerger[RecordOfFieldsAndValues1 <: HList, RecordOfFieldsAndValues2 <: HList]
  (implicit merger: Merger.Aux[RecordOfFieldsAndValues1, RecordOfFieldsAndValues2, RecordOfFieldsAndValues1]): UpdaterFromRecord[RecordOfFieldsAndValues1, RecordOfFieldsAndValues2] =
    new UpdaterFromRecord[RecordOfFieldsAndValues1, RecordOfFieldsAndValues2] {
      def apply(baseRecord: RecordOfFieldsAndValues1, updatedValues: RecordOfFieldsAndValues2): RecordOfFieldsAndValues1 = merger(baseRecord, updatedValues)
    }

  implicit def generateUpdaterForSealedTraitWithNoMember[RecordOfFieldsAndValues <: HList]: UpdaterFromRecord[CNil, RecordOfFieldsAndValues] =
    new UpdaterFromRecord[CNil, RecordOfFieldsAndValues] {
      def apply(t: CNil, r: RecordOfFieldsAndValues): CNil = t
    }

  implicit def recursivelyGenerateUpdaterForGenericRepresentationOfSealedTrait[SealedMember, SealedMembers <: Coproduct, RecordOfFieldsAndValues <: HList]
  (implicit
   updaterForSealedMember: Lazy[UpdaterFromRecord[SealedMember, RecordOfFieldsAndValues]],
   recursiveUpdatedForTheRest: Lazy[UpdaterFromRecord[SealedMembers, RecordOfFieldsAndValues]]
  ): UpdaterFromRecord[SealedMember :+: SealedMembers, RecordOfFieldsAndValues] =
    new UpdaterFromRecord[SealedMember :+: SealedMembers, RecordOfFieldsAndValues] {
      def apply(thingToCopy: SealedMember :+: SealedMembers, newFieldsAndValues: RecordOfFieldsAndValues): SealedMember :+: SealedMembers = thingToCopy match {
        case Inl(head) => Inl(updaterForSealedMember.value(head, newFieldsAndValues))
        case Inr(tail) => Inr(recursiveUpdatedForTheRest.value(tail, newFieldsAndValues))
      }
    }

  implicit def generateUpdaterForGenericRepresentationOfCaseClass[CaseClassToCopy, RecordOfFieldsAndValues <: HList, GenericRepresentationOfTypeToCopy <: HList]
  (implicit
   caseClassChecker: HasProductGeneric[CaseClassToCopy],
   caseClassToGenericTransformer: LabelledGeneric.Aux[CaseClassToCopy, GenericRepresentationOfTypeToCopy],
   updaterForGenericRepresentation: Lazy[UpdaterFromRecord[GenericRepresentationOfTypeToCopy, RecordOfFieldsAndValues]]
  ): UpdaterFromRecord[CaseClassToCopy, RecordOfFieldsAndValues] =
    new UpdaterFromRecord[CaseClassToCopy, RecordOfFieldsAndValues] {
      def apply(thingToCopy: CaseClassToCopy, newFieldsAndValues: RecordOfFieldsAndValues): CaseClassToCopy = {
        val genericRepresentationOfThingToCopy: GenericRepresentationOfTypeToCopy = caseClassToGenericTransformer.to(thingToCopy)
        val genericRepresentationWithUpdatedValues: GenericRepresentationOfTypeToCopy
        = updaterForGenericRepresentation.value(genericRepresentationOfThingToCopy, newFieldsAndValues)
        val finalResultOfCopy: CaseClassToCopy = caseClassToGenericTransformer.from(genericRepresentationWithUpdatedValues)
        finalResultOfCopy
      }
    }

  implicit def generateUpdaterForSealedTrait[SealedTraitToCopy, RecordOfFieldsAndValues <: HList, GenericRepresentationOfSealedTrait <: Coproduct]
  (implicit
   sealedTraitChecker: HasCoproductGeneric[SealedTraitToCopy],
   sealedTraitToGenericTransformer: Generic.Aux[SealedTraitToCopy, GenericRepresentationOfSealedTrait],
   updaterForGenericRepresentation: Lazy[UpdaterFromRecord[GenericRepresentationOfSealedTrait, RecordOfFieldsAndValues]]
  ): UpdaterFromRecord[SealedTraitToCopy, RecordOfFieldsAndValues] =
    new UpdaterFromRecord[SealedTraitToCopy, RecordOfFieldsAndValues] {
      def apply(thingToCopy: SealedTraitToCopy, newFieldsAndValues: RecordOfFieldsAndValues): SealedTraitToCopy = {
        val genericRepresentationOfThingToCopy: GenericRepresentationOfSealedTrait = sealedTraitToGenericTransformer.to(thingToCopy)
        val genericRepresentationWithUpdatedValues: GenericRepresentationOfSealedTrait
        = updaterForGenericRepresentation.value(genericRepresentationOfThingToCopy, newFieldsAndValues)
        val finalResultOfCopy: SealedTraitToCopy = sealedTraitToGenericTransformer.from(genericRepresentationWithUpdatedValues)
        finalResultOfCopy
      }
    }
}
