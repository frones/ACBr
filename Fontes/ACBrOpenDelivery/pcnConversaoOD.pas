{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Gabriel Baltazar                               }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit pcnConversaoOD;

interface

uses
  SysUtils,
  StrUtils,
  TypInfo,
  Classes,
  ACBrUtil.Strings;

type
  TACBrODAllergen = (aAlmonds, aAlphaIsomethylIonone, aAlcohol,
    aAmylCinnamal, aAniseAlcohol, aBarley, aBenzylAlcohol, aBenzylBenzoate,
    aBenzylCinnamate, aBenzylSalicylate, aBrazilNuts, aButylphenylMethylpropionate,
    aCarrots, aCashewNuts, aCelery, aCerealsContainingGluten,
    aCinnamal, aCinnamylAlcohol, aCitral, aCitronellol,
    aCocoa, aCoriander, aCorn, aCoumarin, aCrustaceans, aEggs, aEugenol, aEverniaFurfuracea,
    aEverniaPrunastri, aFarnesol, aFish, aGeraniol, aGluten, aHazelnuts, aHexylCinnamal,
    aHydroxycitronellal, aKamut, aLactose, aLupine, aMacadamiaNuts, aMethyl2Octynoate, aMilk,
    aMolluscs, aMustard, aNoDeclaredAllergens, aOat, aPeanuts, aPeas, aPecanNuts, aPistachios,
    aProdFruits, aQueenslandNuts, aRye, aSesameSeeds, aSoybeans, aSpelt, aSulphurDioxide,
    aTreeNuts, aTreeNutTraces, aWalnuts, aWheat);

  TACBrODAllergenArray = array of TACBrODAllergen;

  TACBrODBrand = (caVisa, caMasterCard, caDiners, caAmex, caHipercard, caElo,
    caAura, caDiscover, caVRBeneficios, caSodexo, caTicket, caGoodCard,
    caBanescard, caSoroCard, caPoliCard, caValeCard, caAgiCard, caJCB,
    caCredSystem, caCabal, caGreenCard, caVeroCheque, caAVista, caOther);

  TACBrODBrandArray = array of TACBrODBrand;

  TACBrODCancelRequestCode = (crcSystemicIssues, crcDuplicateApplication, crcUnavailableItem,
    crcRestaurantWithoutDeliveryMan, crcOutdatedMenu, crcOrderOutsideTheDeliveryArea, crcBlockedCustomer,
    crcOutsideDeliveryHours, crcInternalDifficultiesOfTheRestaurant, crcRiskArea);

  TACBrODCancelRequestMode = (crmAuto, crmManual);

  TACBrODDenyCancelCode = (dccDishAlreadyDone, dccOutForDelivery);

  TACBrODDayOfWeek = (dwMonday, dwTuesday, dwWednesday, dwThursday,
    dwFriday, dwSaturday, dwSunday);

  TACBrODDayOfWeekArray = array of TACBrODDayOfWeek;

  TACBrODDiscountTarget = (dtCart, dtDeliveryFee, dtItem);

  TACBrODEventType = (etCreated, etConfirmed, etDispatched, etReadyForPickup,
    etPickupAreaAssigned, etDelivered, etConcluded, etCancellationRequested,
    etCancellationRequestDenied, etCancelled, etOrderCancellationRequest);

  TACBrODEventTypeArray = array of TACBrODEventType;

  TACBrODFeeReceivedBy = (rbMarketplace, rbMerchant, rbLogisticServices);

  TACBrODFeeType = (ftDeliveryFee, ftServiceFee, ftTip);

  TACBrODIndoorMode = (imDefault, imPlace);

  TACBrODMerchantCategories = (mcBurgers, mcPizza, mcFastFood, mcHotDog,
    mcJapanese, mcDesserts, mcAmerican, mcIceCream, mcBBQ, mcSandwich,
    mcMexican, mcBrazilian, mcPastry, mcArabian, mcComfortFood,
    mcVegetarian, mcVegan, mcBakery, mcHealthy, mcItalian, mcChinese,
    mcJuiceSmoothies, mcSeafood, mcCafe, mcSalads, mcCoffeeTea, mcPasta, mcBreakfastBrunch,
    mcLatinAmerican, mcConvenience, mcPub, mcHawaiian, mcEuropean, mcFamilyMeals, mcFrench,
    mcIndian, mcPortuguese, mcSpanish, mcGourmet, mcKidsFriendly, mcSouthAmerican,
    mcSpecialtyFoods, mcArgentinian, mcPremium, mcAffordableMeals);

  TACBrODMerchantCategoriesArray = array of TACBrODMerchantCategories;

  TACBrODMerchantType = (mtRestaurant);

  TACBrODMerchantUpdateType = (mutEmptyBody, mutOnlyStatus, mutEntityType, mutStatusEntityType);

  TACBrODMerchantUpdateEntity = (mueMerchant, mueBasicInfo, mueService, mueMenu, mueCategory,
    mueItem, mueItemOffer, mueOptionGroup, mueAvailability);

  TACBrODOrderTiming = (otInstant, otScheduled);

  TACBrODOrderTimingArray = array of TACBrODOrderTiming;

  TACBrODPaymentMethod = (pmCredit, pmDebit, pmMealVoucher, pmFoodVoucher,
    pmDigitalWallet, pmPix, pmCash, pmCreditDebit, pmCoupon, pmRedeem,
    pmPrepaidRedeem, pmOther);

  TACBrODPaymentType = (ptPrepaid, ptPending);

  TACBrODScheduleTime = (st15Minutes, st30Minutes, st45Minutes, st60Minutes,
    st90Minutes, st120Minutes);

  TACBrODServiceType = (stDelivery, stTakeout, stIndoor);

  TACBrODSponsor = (sMarketPlace, sMerchant);

  TACBrODStatus = (sAvailable, sUnavailable);

  TACBrODSuitableDiet = (sdDiabetic, sdGlutenFree, sdHalal,
    sdHindu, sdKosher, sdLowCalorie, sdLowFat, sdLowLactose, sdLowSalt, sdVegan,
    sdVegetarian);

  TACBrODSuitableDietArray = array of TACBrODSuitableDiet;

  TACBrODTakeoutMode = (tmDefault, tmPickupArea);

  TACBrODUnit = (unUnit, unKilogram, unLiter, unOunce, unPound, unGallon);

function AllergenToStr(AValue: TACBrODAllergen): string;
function AllergensToArray(AValue: TACBrODAllergenArray): TSplitResult;
function StrToAllergen(const AValue: string): TACBrODAllergen;

function BrandToStr(aValue: TACBrODBrand): String;
function StrToBrand(aValue: String): TACBrODBrand;
function BrandsToArray(aValue: TACBrODBrandArray): TSplitResult;

function CancelRequestCodeToStr(const AValue: TACBrODCancelRequestCode): string;

function CancelRequestModeToStr(const AValue: TACBrODCancelRequestMode): string;

function DenyCancelCodeToStr(const AValue: TACBrODDenyCancelCode): string;

function DayOfWeekToStr(AValue: TACBrODDayOfWeek): string;
function DayOfWeekToArray(AValue: TACBrODDayOfWeekArray): TSplitResult;
function StrToDayOfWeek(const AValue: string): TACBrODDayOfWeek;

function DiscountTargetToStr(AValue: TACBrODDiscountTarget): string;
function StrToDiscountTarget(const AValue: string): TACBrODDiscountTarget;

function EventTypeToStr(AValue: TACBrODEventType): string;
function StrToEventType(const AValue: string): TACBrODEventType;

function FeeReceivedByToStr(AValue: TACBrODFeeReceivedBy): string;
function StrToFeeReceivedBy(const AValue: string): TACBrODFeeReceivedBy;

function FeeTypeToStr(AValue: TACBrODFeeType): string;
function StrToFeeType(const AValue: string): TACBrODFeeType;

function IndoorModeToStr(AValue: TACBrODIndoorMode): string;
function StrToIndoorMode(const AValue: string): TACBrODIndoorMode;

function MerchantCategoriesToStr(AValue: TACBrODMerchantCategories): string;
function MerchantCategoriesToArray(AValue: TACBrODMerchantCategoriesArray): TSplitResult;
function StrToMerchantCategories(const AValue: string): TACBrODMerchantCategories;

function MerchantTypeToStr(AValue: TACBrODMerchantType): string;
function StrToMerchantType(const AValue: string): TACBrODMerchantType;

function OrderTimingToStr(AValue: TACBrODOrderTiming): string;
function OrderTimingToArray(AValue: TACBrODOrderTimingArray): TSplitResult;
function StrToOrderTiming(const AValue: string): TACBrODOrderTiming;

function PaymentMethodToStr(AValue: TACBrODPaymentMethod): string;
function StrToPaymentMethod(const AValue: string): TACBrODPaymentMethod;

function PaymentTypeToStr(AValue: TACBrODPaymentType): string;
function StrToPaymentType(const AValue: string): TACBrODPaymentType;

function ScheduleTimeToStr(AValue: TACBrODScheduleTime): string;
function StrToScheduleTime(AValue: string): TACBrODScheduleTime;

function ServiceTypeToStr(AValue: TACBrODServiceType): string;
function StrToServiceType(AValue: string): TACBrODServiceType;

function SponsorToStr(AValue: TACBrODSponsor): string;
function StrToSponsor(AValue: string): TACBrODSponsor;

function StatusToStr(AValue: TACBrODStatus): string;
function StrToStatus(AValue: string): TACBrODStatus;

function SuitableDietToStr(AValue: TACBrODSuitableDiet): string;
function SuitableDietToArray(AValue: TACBrODSuitableDietArray): TSplitResult;
function StrToSuitableDiet(const AValue: string): TACBrODSuitableDiet;

function TakeoutModeToStr(AValue: TACBrODTakeoutMode): string;
function StrToTakeoutMode(AValue: string): TACBrODTakeoutMode;

function UnitToStr(aValue: TACBrODUnit): String;
function StrToUnit(aValue: String): TACBrODUnit;

implementation

function AllergenToStr(AValue: TACBrODAllergen): string;
begin
  case AValue of
    aAlmonds: Result := 'ALMONDS';
    aAlphaIsomethylIonone: Result := 'ALPHA_ISOMETHYL_IONONE';
    aAlcohol: Result := 'ALCOHOL';
    aAmylCinnamal: Result := 'AMYL_CINNAMAL';
    aAniseAlcohol: Result := 'ANISE_ALCOHOL';
    aBarley: Result := 'BARLEY';
    aBenzylAlcohol: Result := 'BENZYL_ALCOHOL';
    aBenzylBenzoate: Result := 'BENZYL_BENZOATE';
    aBenzylCinnamate: Result := 'BENZYL_CINNAMATE';
    aBenzylSalicylate: Result := 'BENZYL_SALICYLATE';
    aBrazilNuts: Result := 'BRAZIL_NUTS';
    aButylphenylMethylpropionate: Result := 'BUTYLPHENYL_METHYLPROPIONATE';
    aCarrots: Result := 'CARROTS';
    aCashewNuts: Result := 'CASHEW_NUTS';
    aCelery: Result := 'CELERY';
    aCerealsContainingGluten: Result := 'CEREALS_CONTAINING_GLUTEN';
    aCinnamal: Result := 'CINNAMAL';
    aCinnamylAlcohol: Result := 'CINNAMYL_ALCOHOL';
    aCitral: Result := 'CITRAL';
    aCitronellol: Result := 'CITRONELLOL';
    aCocoa: Result := 'COCOA';
    aCoriander: Result := 'CORIANDER';
    aCorn: Result := 'CORN';
    aCoumarin: Result := 'COUMARIN';
    aCrustaceans: Result := 'CRUSTACEANS';
    aEggs: Result := 'EGGS';
    aEugenol: Result := 'EUGENOL';
    aEverniaFurfuracea: Result := 'EVERNIA_FURFURACEA';
    aEverniaPrunastri: Result := 'EVERNIA_PRUNASTRI';
    aFarnesol: Result := 'FARNESOL';
    aFish: Result := 'FISH';
    aGeraniol: Result := 'GERANIOL';
    aGluten: Result := 'GLUTEN';
    aHazelnuts: Result := 'HAZELNUTS';
    aHexylCinnamal: Result := 'HEXYL_CINNAMAL';
    aHydroxycitronellal: Result := 'HYDROXYCITRONELLAL';
    aKamut: Result := 'KAMUT';
    aLactose: Result := 'LACTOSE';
    aLupine: Result := 'LUPINE';
    aMacadamiaNuts: Result := 'MACADAMIA_NUTS';
    aMethyl2Octynoate: Result := 'METHYL_2_OCTYNOATE';
    aMilk: Result := 'MILK';
    aMolluscs: Result := 'MOLLUSCS';
    aMustard: Result := 'MUSTARD';
    aNoDeclaredAllergens: Result := 'NO_DECLARED_ALLERGENS';
    aOat: Result := 'OAT';
    aPeanuts: Result := 'PEANUTS';
    aPeas: Result := 'PEAS';
    aPecanNuts: Result := 'PECAN_NUTS';
    aPistachios: Result := 'PISTACHIOS';
    aProdFruits: Result := 'POD_FRUITS';
    aQueenslandNuts: Result := 'QUEENSLAND_NUTS';
    aRye: Result := 'RYE';
    aSesameSeeds: Result := 'SESAME_SEEDS';
    aSoybeans: Result := 'SOYBEANS';
    aSpelt: Result := 'SPELT';
    aSulphurDioxide: Result := 'SULPHUR_DIOXIDE';
    aTreeNuts: Result := 'TREE_NUTS';
    aTreeNutTraces: Result := 'TREE_NUT_TRACES';
    aWalnuts: Result := 'WALNUTS';
    aWheat: Result := 'WHEAT';
  else
    Result := '';
  end;
end;

function AllergensToArray(AValue: TACBrODAllergenArray): TSplitResult;
var
  I: Integer;
begin
  SetLength(Result, Length(AValue));
  for I := 0 to Pred(Length(AValue)) do
    Result[I] := AllergenToStr(AValue[I]);
end;

function StrToAllergen(const AValue: string): TACBrODAllergen;
var
  LStr: string;
begin
  Result := aAlmonds;
  LStr := UpperCase(AValue);
  if LStr = 'ALMONDS' then
    Result := aAlmonds
  else if LStr = 'ALPHA_ISOMETHYL_IONONE' then
    Result := aAlphaIsomethylIonone
  else if LStr = 'ALCOHOL' then
    Result := aAlcohol
  else if LStr = 'AMYL_CINNAMAL' then
    Result := aAmylCinnamal
  else if LStr = 'ANISE_ALCOHOL' then
    Result := aAniseAlcohol
  else if LStr = 'BARLEY' then
    Result := aBarley
  else if LStr = 'BENZYL_ALCOHOL' then
    Result := aBenzylAlcohol
  else if LStr = 'BENZYL_BENZOATE' then
    Result := aBenzylBenzoate
  else if LStr = 'BENZYL_CINNAMATE' then
    Result := aBenzylCinnamate
  else if LStr = 'BENZYL_SALICYLATE' then
    Result := aBenzylSalicylate
  else if LStr = 'BRAZIL_NUTS' then
    Result := aBrazilNuts
  else if LStr = 'BUTYLPHENYL_METHYLPROPIONATE' then
    Result := aButylphenylMethylpropionate
  else if LStr = 'CARROTS' then
    Result := aCarrots
  else if LStr = 'CASHEW_NUTS' then
    Result := aCashewNuts
  else if LStr = 'CELERY' then
    Result := aCelery
  else if LStr = 'CEREALS_CONTAINING_GLUTEN' then
    Result := aCerealsContainingGluten
  else if LStr = 'CINNAMAL' then
    Result := aCinnamal
  else if LStr = 'CINNAMYL_ALCOHOL' then
    Result := aCinnamylAlcohol
  else if LStr = 'CITRAL' then
    Result := aCitral
  else if LStr = 'CITRONELLOL' then
    Result := aCitronellol
  else if LStr = 'COCOA' then
    Result := aCocoa
  else if LStr = 'CORIANDER' then
    Result := aCoriander
  else if LStr = 'CORN' then
    Result := aCorn
  else if LStr = 'COUMARIN' then
    Result := aCoumarin
  else if LStr = 'CRUSTACEANS' then
    Result := aCrustaceans
  else if LStr = 'EGGS' then
    Result := aEggs
  else if LStr = 'EUGENOL' then
    Result := aEugenol
  else if LStr = 'EVERNIA_FURFURACEA' then
    Result := aEverniaFurfuracea
  else if LStr = 'EVERNIA_PRUNASTRI' then
    Result := aEverniaPrunastri
  else if LStr = 'FARNESOL' then
    Result := aFarnesol
  else if LStr = 'FISH' then
    Result := aFish
  else if LStr = 'GERANIOL' then
    Result := aGeraniol
  else if LStr = 'GLUTEN' then
    Result := aGluten
  else if LStr = 'HAZELNUTS' then
    Result := aHazelnuts
  else if LStr = 'HEXYL_CINNAMAL' then
    Result := aHexylCinnamal
  else if LStr = 'HYDROXYCITRONELLAL' then
    Result := aHydroxycitronellal
  else if LStr = 'KAMUT' then
    Result := aKamut
  else if LStr = 'LACTOSE' then
    Result := aLactose
  else if LStr = 'LUPINE' then
    Result := aLupine
  else if LStr = 'MACADAMIA_NUTS' then
    Result := aMacadamiaNuts
  else if LStr = 'METHYL_2_OCTYNOATE' then
    Result := aMethyl2Octynoate
  else if LStr = 'MILK' then
    Result := aMilk
  else if LStr = 'MOLLUSCS' then
    Result := aMolluscs
  else if LStr = 'MUSTARD' then
    Result := aMustard
  else if LStr = 'NO_DECLARED_ALLERGENS' then
    Result := aNoDeclaredAllergens
  else if LStr = 'OAT' then
    Result := aOat
  else if LStr = 'PEANUTS' then
    Result := aPeanuts
  else if LStr = 'PEAS' then
    Result := aPeas
  else if LStr = 'PECAN_NUTS' then
    Result := aPecanNuts
  else if LStr = 'PISTACHIOS' then
    Result := aPistachios
  else if LStr = 'POD_FRUITS' then
    Result := aProdFruits
  else if LStr = 'QUEENSLAND_NUTS' then
    Result := aQueenslandNuts
  else if LStr = 'RYE' then
    Result := aRye
  else if LStr = 'SESAME_SEEDS' then
    Result := aSesameSeeds
  else if LStr = 'SOYBEANS' then
    Result := aSoybeans
  else if LStr = 'SPELT' then
    Result := aSpelt
  else if LStr = 'SULPHUR_DIOXIDE' then
    Result := aSulphurDioxide
  else if LStr = 'TREE_NUTS' then
    Result := aTreeNuts
  else if LStr = 'TREE_NUT_TRACES' then
    Result := aTreeNutTraces
  else if LStr = 'WALNUTS' then
    Result := aWalnuts
  else if LStr = 'WHEAT' then
    Result := aWheat;
end;

function BrandToStr(aValue: TACBrODBrand): String;
begin
  case aValue of
    caVisa: Result := 'VISA';
    caMasterCard: Result := 'MASTERCARD';
    caDiners: Result := 'DINERS';
    caAmex: Result := 'AMEX';
    caHipercard: Result := 'HIPERCARD';
    caElo: Result := 'ELO';
    caAura: Result := 'AURA';
    caDiscover: Result := 'DISCOVER';
    caVRBeneficios: Result := 'VR_BENEFICIOS';
    caSodexo: Result := 'SODEXO';
    caTicket: Result := 'TICKET';
    caGoodCard: Result := 'GOOD_CARD';
    caBanescard: Result := 'BANESCARD';
    caSoroCard: Result := 'SOROCARD';
    caPoliCard: Result := 'POLICARD';
    caValeCard: Result := 'VALECARD';
    caAgiCard: Result := 'AGICARD';
    caJCB: Result := 'JCB';
    caCredSystem: Result := 'CREDSYSTEM';
    caCabal: Result := 'CABAL';
    caGreenCard: Result := 'GREEN_CARD';
    caVeroCheque: Result := 'VEROCHEQUE';
    caAVista: Result := 'AVISTA';
    caOther: Result := 'OTHER';
  else
    Result := EmptyStr;
  end;
end;

function StrToBrand(aValue: String): TACBrODBrand;
var
  wUpStr: String;
begin
  Result := caOther;
  wUpStr := UpperCase(aValue);

  if wUpStr = 'VISA' then
    Result := caVisa
  else if wUpStr = 'MASTERCARD' then
    Result := caMasterCard
  else if wUpStr = 'DINERS' then
    Result := caDiners
  else if wUpStr = 'AMEX' then
    Result := caAmex
  else if wUpStr = 'HIPERCARD' then
    Result := caHipercard
  else if wUpStr = 'ELO' then
    Result := caElo
  else if wUpStr = 'AURA' then
    Result := caAura
  else if wUpStr = 'DISCOVER' then
    Result := caDiscover
  else if wUpStr = 'VR_BENEFICIOS' then
    Result := caVRBeneficios
  else if wUpStr = 'SODEXO' then
    Result := caSodexo
  else if wUpStr = 'TICKET' then
    Result := caTicket
  else if wUpStr = 'GOOD_CARD' then
    Result := caGoodCard
  else if wUpStr = 'BANESCARD' then
    Result := caBanescard
  else if wUpStr = 'SOROCARD' then
    Result := caSoroCard
  else if wUpStr = 'POLICARD' then
    Result := caPoliCard
  else if wUpStr = 'VALECARD' then
    Result := caValeCard
  else if wUpStr = 'AGICARD' then
    Result := caAgiCard
  else if wUpStr = 'JCB' then
    Result := caJCB
  else if wUpStr = 'CREDSYSTEM' then
    Result := caCredSystem
  else if wUpStr = 'CABAL' then
    Result := caCabal
  else if wUpStr = 'GREEN_CARD' then
    Result := caGreenCard
  else if wUpStr = 'VEROCHEQUE' then
    Result := caVeroCheque
  else if wUpStr = 'AVISTA' then
    Result := caAVista;
end;

function BrandsToArray(aValue: TACBrODBrandArray): TSplitResult;
var
  I: Integer;
begin
  SetLength(Result, Length(aValue));
  for I := 0 to Pred(Length(aValue)) do
    Result[I] := BrandToStr(aValue[I]);
end;

function CancelRequestCodeToStr(const AValue: TACBrODCancelRequestCode): string;
begin
  case AValue of
    crcSystemicIssues: Result := 'SYSTEMIC_ISSUES';
    crcDuplicateApplication: Result := 'DUPLICATE_APPLICATION';
    crcUnavailableItem: Result := 'UNAVAILABLE_ITEM';
    crcRestaurantWithoutDeliveryMan: Result := 'RESTAURANT_WITHOUT_DELIVERY_MAN';
    crcOutdatedMenu: Result := 'OUTDATED_MENU';
    crcOrderOutsideTheDeliveryArea: Result := 'ORDER_OUTSIDE_THE_DELIVERY_AREA';
    crcBlockedCustomer: Result := 'BLOCKED_CUSTOMER';
    crcOutsideDeliveryHours: Result := 'OUTSIDE_DELIVERY_HOURS';
    crcInternalDifficultiesOfTheRestaurant: Result := 'INTERNAL_DIFFICULTIES_OF_THE_RESTAURANT';
    crcRiskArea: Result := 'RISK_AREA';
  else
    Result := '';
  end;
end;

function CancelRequestModeToStr(const AValue: TACBrODCancelRequestMode): string;
begin
  case AValue of
    crmAuto: Result := 'AUTO';
    crmManual: Result := 'MANUAL';
  else
    Result := '';
  end;
end;

function DenyCancelCodeToStr(const AValue: TACBrODDenyCancelCode): string;
begin
  case AValue of
    dccDishAlreadyDone: Result := 'DISH_ALREADY_DONE';
    dccOutForDelivery: Result := 'OUT_FOR_DELIVERY';
  else
    Result := '';
  end;
end;

function DayOfWeekToStr(AValue: TACBrODDayOfWeek): string;
begin
  case AValue of
    dwMonday: Result := 'MONDAY';
    dwTuesday: Result := 'TUESDAY';
    dwWednesday: Result := 'WEDNESDAY';
    dwThursday: Result := 'THURSDAY';
    dwFriday: Result := 'FRIDAY';
    dwSaturday: Result := 'SATURDAY';
    dwSunday: Result := 'SUNDAY';
  else
    Result := '';
  end;
end;

function DayOfWeekToArray(AValue: TACBrODDayOfWeekArray): TSplitResult;
var
  I: Integer;
begin
  SetLength(Result, Length(AValue));
  for I := 0 to Pred(Length(AValue)) do
    Result[I] := DayOfWeekToStr(AValue[I]);
end;

function StrToDayOfWeek(const AValue: string): TACBrODDayOfWeek;
var
  LStr: string;
begin
  Result := dwMonday;
  LStr := UpperCase(AValue);
  if LStr = 'MONDAY' then
    Result := dwMonday
  else if LStr = 'TUESDAY' then
    Result := dwTuesday
  else if LStr = 'WEDNESDAY' then
    Result := dwWednesday
  else if LStr = 'THURSDAY' then
    Result := dwThursday
  else if LStr = 'FRIDAY' then
    Result := dwFriday
  else if LStr = 'SATURDAY' then
    Result := dwSaturday
  else if LStr = 'SUNDAY' then
    Result := dwSunday;
end;

function DiscountTargetToStr(AValue: TACBrODDiscountTarget): string;
begin
  case AValue of
    dtCart: Result := 'CART';
    dtDeliveryFee: Result := 'DELIVERY_FEE';
    dtItem: Result := 'ITEM';
  else
    Result := '';
  end;
end;

function StrToDiscountTarget(const AValue: string): TACBrODDiscountTarget;
var
  LStr: string;
begin
  Result := dtCart;
  LStr := UpperCase(AValue);
  if LStr = 'CART' then
    Result := dtCart
  else if LStr = 'DELIVERY_FEE' then
    Result := dtDeliveryFee
  else if LStr = 'ITEM' then
    Result := dtItem;
end;

function EventTypeToStr(AValue: TACBrODEventType): string;
begin
  case AValue of
    etCreated: Result := 'CREATED';
    etConfirmed: Result := 'CONFIRMED';
    etDispatched: Result := 'DISPATCHED';
    etReadyForPickup: Result := 'READY_FOR_PICKUP';
    etPickupAreaAssigned: Result := 'PICKUP_AREA_ASSIGNED';
    etDelivered: Result := 'DELIVERED';
    etConcluded: Result := 'CONCLUDED';
    etCancellationRequested: Result := 'CANCELLATION_REQUESTED';
    etCancellationRequestDenied: Result := 'CANCELLATION_REQUEST_DENIED';
    etCancelled: Result := 'CANCELLED';
    etOrderCancellationRequest: Result := 'ORDER_CANCELLATION_REQUEST';
  else
    Result := '';
  end;
end;

function StrToEventType(const AValue: string): TACBrODEventType;
var
  LStr: string;
begin
  LStr := UpperCase(AValue);
  Result := etCreated;
  if LStr = 'CREATED' then
    Result := etCreated
  else if LStr = 'CONFIRMED' then
    Result := etConfirmed
  else if LStr = 'DISPATCHED' then
    Result := etDispatched
  else if LStr = 'READY_FOR_PICKUP' then
    Result := etReadyForPickup
  else if LStr = 'PICKUP_AREA_ASSIGNED' then
    Result := etPickupAreaAssigned
  else if LStr = 'DELIVERED' then
    Result := etDelivered
  else if LStr = 'CONCLUDED' then
    Result := etConcluded
  else if LStr = 'CANCELLATION_REQUESTED' then
    Result := etCancellationRequested
  else if LStr = 'CANCELLATION_REQUEST_DENIED' then
    Result := etCancellationRequestDenied
  else if LStr = 'CANCELLED' then
    Result := etCancelled
  else if LStr = 'ORDER_CANCELLATION_REQUEST' then
    Result := etOrderCancellationRequest;
end;

function FeeReceivedByToStr(AValue: TACBrODFeeReceivedBy): string;
begin
  case AValue of
    rbMarketplace: Result := 'MARKETPLACE';
    rbMerchant: Result := 'MERCHANT';
    rbLogisticServices: Result := 'LOGISTIC_SERVICES';
  else
    Result := '';
  end;
end;

function StrToFeeReceivedBy(const AValue: string): TACBrODFeeReceivedBy;
var
  LStr: string;
begin
  Result := rbMarketplace;
  LStr := UpperCase(AValue);
  if LStr = 'MARKETPLACE' then
    Result := rbMarketplace
  else if LStr = 'MERCHANT' then
    Result := rbMerchant
  else if LStr = 'LOGISTIC_SERVICES' then
    Result := rbLogisticServices;
end;

function FeeTypeToStr(AValue: TACBrODFeeType): string;
begin
  case AValue of
    ftDeliveryFee: Result := 'DELIVERY_FEE';
    ftServiceFee: Result := 'SERVICE_FEE';
    ftTip: Result := 'TIP';
  else
    Result := '';
  end;
end;

function StrToFeeType(const AValue: string): TACBrODFeeType;
var
  LStr: string;
begin
  Result := ftDeliveryFee;
  LStr := UpperCase(AValue);
  if LStr = 'DELIVERY_FEE' then
    Result := ftDeliveryFee
  else if LStr = 'SERVICE_FEE' then
    Result := ftServiceFee
  else if LStr = 'TIP' then
    Result := ftTip;
end;

function IndoorModeToStr(AValue: TACBrODIndoorMode): string;
begin
  case AValue of
    imDefault: Result := 'DEFAULT';
    imPlace: Result := 'PLACE';
  else
    Result := '';
  end;
end;

function StrToIndoorMode(const AValue: string): TACBrODIndoorMode;
var
  LStr: string;
begin
  Result := imDefault;
  LStr := UpperCase(AValue);
  if LStr = 'DEFAULT' then
    Result := imDefault
  else if LStr = 'PLACE' then
    Result := imPlace;
end;

function MerchantCategoriesToStr(AValue: TACBrODMerchantCategories): string;
begin
  case AValue of
    mcBurgers: Result := 'BURGERS';
    mcPizza: Result := 'PIZZA';
    mcFastFood: Result := 'FAST_FOOD';
    mcHotDog: Result := 'HOT_DOG';
    mcJapanese: Result := 'JAPANESE';
    mcDesserts: Result := 'DESSERTS';
    mcAmerican: Result := 'AMERICAN';
    mcIceCream: Result := 'ICE_CREAM';
    mcBBQ: Result := 'BBQ';
    mcSandwich: Result := 'SANDWICH';
    mcMexican: Result := 'MEXICAN';
    mcBrazilian: Result := 'BRAZILIAN';
    mcPastry: Result := 'PASTRY';
    mcArabian: Result := 'ARABIAN';
    mcComfortFood: Result := 'COMFORT_FOOD';
    mcVegetarian: Result := 'VEGETARIAN';
    mcVegan: Result := 'VEGAN';
    mcBakery: Result := 'BAKERY';
    mcHealthy: Result := 'HEALTHY';
    mcItalian: Result := 'ITALIAN';
    mcChinese: Result := 'CHINESE';
    mcJuiceSmoothies: Result := 'JUICE_SMOOTHIES';
    mcSeafood: Result := 'SEAFOOD';
    mcCafe: Result := 'CAFE';
    mcSalads: Result := 'SALADS';
    mcCoffeeTea: Result := 'COFFEE_TEA';
    mcPasta: Result := 'PASTA';
    mcBreakfastBrunch: Result := 'BREAKFAST_BRUNCH';
    mcLatinAmerican: Result := 'LATIN_AMERICAN';
    mcConvenience: Result := 'CONVENIENCE';
    mcPub: Result := 'PUB';
    mcHawaiian: Result := 'HAWAIIAN';
    mcEuropean: Result := 'EUROPEAN';
    mcFamilyMeals: Result := 'FAMILY_MEALS';
    mcFrench: Result := 'FRENCH';
    mcIndian: Result := 'INDIAN';
    mcPortuguese: Result := 'PORTUGUESE';
    mcSpanish: Result := 'SPANISH';
    mcGourmet: Result := 'GOURMET';
    mcKidsFriendly: Result := 'KIDS_FRIENDLY';
    mcSouthAmerican: Result := 'SOUTH_AMERICAN';
    mcSpecialtyFoods: Result := 'SPECIALTY_FOODS';
    mcArgentinian: Result := 'ARGENTINIAN';
    mcPremium: Result := 'PREMIUM';
    mcAffordableMeals: Result := 'AFFORDABLE_MEALS';
  else
    Result := '';
  end;
end;

function MerchantCategoriesToArray(AValue: TACBrODMerchantCategoriesArray): TSplitResult;
var
  I: Integer;
begin
  SetLength(Result, Length(AValue));
  for I := 0 to Pred(Length(AValue)) do
    Result[I] := MerchantCategoriesToStr(AValue[I]);
end;

function StrToMerchantCategories(const AValue: string): TACBrODMerchantCategories;
var
  LStr: string;
begin
  Result := mcBurgers;
  LStr := UpperCase(AValue);
  if LStr = 'BURGERS' then
    Result := mcBurgers
  else if LStr = 'PIZZA' then
    Result := mcPizza
  else if LStr = 'FAST_FOOD' then
    Result := mcFastFood
  else if LStr = 'HOT_DOG' then
    Result := mcHotDog
  else if LStr = 'JAPANESE' then
    Result := mcJapanese
  else if LStr = 'DESSERTS' then
    Result := mcDesserts
  else if LStr = 'AMERICAN' then
    Result := mcAmerican
  else if LStr = 'ICE_CREAM' then
    Result := mcIceCream
  else if LStr = 'BBQ' then
    Result := mcBBQ
  else if LStr = 'SANDWICH' then
    Result := mcSandwich
  else if LStr = 'MEXICAN' then
    Result := mcMexican
  else if LStr = 'BRAZILIAN' then
    Result := mcBrazilian
  else if LStr = 'PASTRY' then
    Result := mcPastry
  else if LStr = 'ARABIAN' then
    Result := mcArabian
  else if LStr = 'COMFORT_FOOD' then
    Result := mcComfortFood
  else if LStr = 'VEGETARIAN' then
    Result := mcVegetarian
  else if LStr = 'VEGAN' then
    Result := mcVegan
  else if LStr = 'BAKERY' then
    Result := mcBakery
  else if LStr = 'HEALTHY' then
    Result := mcHealthy
  else if LStr = 'ITALIAN' then
    Result := mcItalian
  else if LStr = 'CHINESE' then
    Result := mcChinese
  else if LStr = 'JUICE_SMOOTHIES' then
    Result := mcJuiceSmoothies
  else if LStr = 'SEAFOOD' then
    Result := mcSeafood
  else if LStr = 'CAFE' then
    Result := mcCafe
  else if LStr = 'SALADS' then
    Result := mcSalads
  else if LStr = 'COFFEE_TEA' then
    Result := mcCoffeeTea
  else if LStr = 'PASTA' then
    Result := mcPasta
  else if LStr = 'BREAKFAST_BRUNCH' then
    Result := mcBreakfastBrunch
  else if LStr = 'LATIN_AMERICAN' then
    Result := mcLatinAmerican
  else if LStr = 'CONVENIENCE' then
    Result := mcConvenience
  else if LStr = 'PUB' then
    Result := mcPub
  else if LStr = 'HAWAIIAN' then
    Result := mcHawaiian
  else if LStr = 'EUROPEAN' then
    Result := mcEuropean
  else if LStr = 'FAMILY_MEALS' then
    Result := mcFamilyMeals
  else if LStr = 'FRENCH' then
    Result := mcFrench
  else if LStr = 'INDIAN' then
    Result := mcIndian
  else if LStr = 'PORTUGUESE' then
    Result := mcPortuguese
  else if LStr = 'SPANISH' then
    Result := mcSpanish
  else if LStr = 'GOURMET' then
    Result := mcGourmet
  else if LStr = 'KIDS_FRIENDLY' then
    Result := mcKidsFriendly
  else if LStr = 'SOUTH_AMERICAN' then
    Result := mcSouthAmerican
  else if LStr = 'SPECIALTY_FOODS' then
    Result := mcSpecialtyFoods
  else if LStr = 'ARGENTINIAN' then
    Result := mcArgentinian
  else if LStr = 'PREMIUM' then
    Result := mcPremium
  else if LStr = 'AFFORDABLE_MEALS' then
    Result := mcAffordableMeals;
end;

function MerchantTypeToStr(AValue: TACBrODMerchantType): string;
begin
  case AValue of
    mtRestaurant: Result := 'RESTAURANT';
  else
    Result := '';
  end;
end;

function StrToMerchantType(const AValue: string): TACBrODMerchantType;
begin
  Result := mtRestaurant;
end;

function OrderTimingToStr(AValue: TACBrODOrderTiming): string;
begin
  case AValue of
    otInstant: Result := 'INSTANT';
    otScheduled: Result := 'SCHEDULED';
  else
    Result := '';
  end;
end;

function OrderTimingToArray(AValue: TACBrODOrderTimingArray): TSplitResult;
var
  I: Integer;
begin
  SetLength(Result, Length(AValue));
  for I := 0 to Pred(Length(AValue)) do
    Result[I] := OrderTimingToStr(AValue[I]);
end;

function StrToOrderTiming(const AValue: string): TACBrODOrderTiming;
var
  LStr: string;
begin
  Result := otInstant;
  LStr := UpperCase(AValue);
  if LStr = 'INSTANT' then
    Result := otInstant
  else if LStr = 'SCHEDULED' then
    Result := otScheduled;
end;

function PaymentMethodToStr(AValue: TACBrODPaymentMethod): string;
begin
  case AValue of
    pmCredit: Result := 'CREDIT';
    pmDebit: Result := 'DEBIT';
    pmMealVoucher: Result := 'MEAL_VOUCHER';
    pmFoodVoucher: Result := 'FOOD_VOUCHER';
    pmDigitalWallet: Result := 'DIGITAL_WALLET';
    pmPix: Result := 'PIX';
    pmCash: Result := 'CASH';
    pmCreditDebit: Result := 'CREDIT_DEBIT';
    pmCoupon: Result := 'COUPON';
    pmRedeem: Result := 'REDEEM';
    pmPrepaidRedeem: Result := 'PREPAID_REDEEM';
    pmOther: Result := 'OTHER';
  else
    Result := ''
  end;
end;

function StrToPaymentMethod(const AValue: string): TACBrODPaymentMethod;
var
  LStr: string;
begin
  Result := pmCredit;
  LStr := UpperCase(AValue);
  if LStr = 'CREDIT' then
    Result := pmCredit
  else if LStr = 'DEBIT' then
    Result := pmDebit
  else if LStr = 'MEAL_VOUCHER' then
    Result := pmMealVoucher
  else if LStr = 'FOOD_VOUCHER' then
    Result := pmFoodVoucher
  else if LStr = 'DIGITAL_WALLET' then
    Result := pmDigitalWallet
  else if LStr = 'PIX' then
    Result := pmPix
  else if LStr = 'CASH' then
    Result := pmCash
  else if LStr = 'CREDIT_DEBIT' then
    Result := pmCreditDebit
  else if LStr = 'COUPON' then
    Result := pmCoupon
  else if LStr = 'REDEEM' then
    Result := pmRedeem
  else if LStr = 'PREPAID_REDEEM' then
    Result := pmPrepaidRedeem
  else if LStr = 'OTHER' then
    Result := pmOther;
end;

function PaymentTypeToStr(AValue: TACBrODPaymentType): string;
begin
  case AValue of
    ptPrepaid: Result := 'PREPAID';
    ptPending: Result := 'PENDING';
  else
    Result := '';
  end;
end;

function StrToPaymentType(const AValue: string): TACBrODPaymentType;
var
  LStr: string;
begin
  Result := ptPrepaid;
  LStr := UpperCase(AValue);
  if LStr = 'PREPAID' then
    Result := ptPrepaid
  else if LStr = 'PENDING' then
    Result := ptPending;
end;

function ScheduleTimeToStr(AValue: TACBrODScheduleTime): string;
begin
  case AValue of
    st15Minutes: Result := '15_MINUTES';
    st30Minutes: Result := '30_MINUTES';
    st45Minutes: Result := '45_MINUTES';
    st60Minutes: Result := '60_MINUTES';
    st90Minutes: Result := '90_MINUTES';
    st120Minutes: Result := '120_MINUTES';
  else
    Result := '';
  end;
end;

function StrToScheduleTime(AValue: string): TACBrODScheduleTime;
var
  LStr: string;
begin
  Result := st120Minutes;
  LStr := UpperCase(AValue);
  if LStr = '15_MINUTES' then
    Result := st15Minutes
  else if LStr = '30_MINUTES' then
    Result := st30Minutes
  else if LStr = '45_MINUTES' then
    Result := st45Minutes
  else if LStr = '60_MINUTES' then
    Result := st60Minutes
  else if LStr = '90_MINUTES' then
    Result := st90Minutes
  else if LStr = '120_MINUTES' then
    Result := st120Minutes;
end;

function ServiceTypeToStr(AValue: TACBrODServiceType): string;
begin
  case AValue of
    stDelivery: Result := 'DELIVERY';
    stTakeout: Result := 'TAKEOUT';
    stIndoor: Result := 'INDOOR';
  else
    Result := '';
  end;
end;

function StrToServiceType(AValue: string): TACBrODServiceType;
var
  LStr: string;
begin
  Result := stDelivery;
  LStr := UpperCase(AValue);
  if LStr = 'DELIVERY' then
    Result := stDelivery
  else if LStr = 'TAKEOUT' then
    Result := stTakeout
  else if LStr = 'INDOOR' then
    Result := stIndoor;
end;

function SponsorToStr(AValue: TACBrODSponsor): string;
begin
  case AValue of
    sMarketPlace: Result := 'MARKETPLACE';
    sMerchant: Result := 'MERCHANT';
  else
    Result := '';
  end;
end;

function StrToSponsor(AValue: string): TACBrODSponsor;
var
  LStr: string;
begin
  Result := sMarketPlace;
  LStr := UpperCase(AValue);
  if LStr = 'MARKETPLACE' then
    Result := sMarketPlace
  else if LStr = 'MERCHANT' then
    Result := sMerchant;
end;

function StatusToStr(AValue: TACBrODStatus): string;
begin
  case AValue of
    sAvailable: Result := 'AVAILABLE';
    sUnavailable: Result := 'UNAVAILABLE';
  else
    Result := '';
  end;
end;

function StrToStatus(AValue: string): TACBrODStatus;
var
  LStr: string;
begin
  Result := sAvailable;
  LStr := UpperCase(AValue);
  if LStr = 'AVAILABLE' then
    Result := sAvailable
  else if LStr = 'UNAVAILABLE' then
    Result := sUnavailable;
end;

function SuitableDietToStr(AValue: TACBrODSuitableDiet): string;
begin
  case AValue of
    sdDiabetic: Result := 'DIABETIC';
    sdGlutenFree: Result := 'GLUTEN_FREE';
    sdHalal: Result := 'HALAL';
    sdHindu: Result := 'HINDU';
    sdKosher: Result := 'KOSHER';
    sdLowCalorie: Result := 'LOW_CALORIE';
    sdLowFat: Result := 'LOW_FAT';
    sdLowLactose: Result := 'LOW_LACTOSE';
    sdLowSalt: Result := 'LOW_SALT';
    sdVegan: Result := 'VEGAN';
    sdVegetarian: Result := 'VEGETARIAN';
  else
    Result := '';
  end;
end;

function SuitableDietToArray(AValue: TACBrODSuitableDietArray): TSplitResult;
var
  I: Integer;
begin
  SetLength(Result, Length(AValue));
  for I := 0 to Pred(Length(AValue)) do
    Result[I] := SuitableDietToStr(AValue[I]);
end;

function StrToSuitableDiet(const AValue: string): TACBrODSuitableDiet;
var
  LStr: string;
begin
  Result := sdDiabetic;
  LStr := UpperCase(AValue);
  if LStr = 'DIABETIC' then
    Result := sdDiabetic
  else if LStr = 'GLUTEN_FREE' then
    Result := sdGlutenFree
  else if LStr = 'HALAL' then
    Result := sdHalal
  else if LStr = 'HINDU' then
    Result := sdHindu
  else if LStr = 'KOSHER' then
    Result := sdKosher
  else if LStr = 'LOW_CALORIE' then
    Result := sdLowCalorie
  else if LStr = 'LOW_FAT' then
    Result := sdLowFat
  else if LStr = 'LOW_LACTOSE' then
    Result := sdLowLactose
  else if LStr = 'LOW_SALT' then
    Result := sdLowSalt
  else if LStr = 'VEGAN' then
    Result := sdVegan
  else if LStr = 'VEGETARIAN' then
    Result := sdVegetarian;
end;

function TakeoutModeToStr(AValue: TACBrODTakeoutMode): string;
begin
  case AValue of
    tmDefault: Result := 'DEFAULT';
    tmPickupArea: Result := 'PICKUP_AREA';
  else
    Result := ''
  end;
end;

function StrToTakeoutMode(AValue: string): TACBrODTakeoutMode;
var
  LStr: string;
begin
  Result := tmDefault;
  LStr := UpperCase(AValue);
  if LStr = 'DEFAULT' then
    Result := tmDefault
  else if LStr = 'PICKUP_AREA' then
    Result := tmPickupArea
end;

function UnitToStr(aValue: TACBrODUnit): String;
begin
  case aValue of
    unKilogram: Result := 'KG';
    unLiter: Result := 'L';
    unOunce: Result := 'OZ';
    unPound: Result := 'LB';
    unGallon: Result := 'GAL';
  else
    Result := 'UN';
  end;
end;

function StrToUnit(aValue: String): TACBrODUnit;
var
  wUpStr: String;
begin
  Result := unUnit;
  wUpStr := UpperCase(aValue);
  if (wUpStr = 'KG') then
    Result := unKilogram
  else if (wUpStr = 'L') then
    Result := unLiter
  else if (wUpStr = 'OZ') then
    Result := unOunce
  else if (wUpStr = 'LB') then
    Result := unPound
  else if (wUpStr = 'GAL') then
    Result := unGallon;
end;

end.
