unit ACBrNFeDMLaz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Dialogs, LR_Class,
  LR_DSet, pcnNFe, pcnConversao;

type

  { TdmACBrNFe }

  TdmACBrNFe = class(TDataModule)
    frReport1: TfrReport;
    CustomIdentificacaoCXN: TfrUserDataset;
    CustomVeiculoCXN: TfrUserDataset;
    CustomVolumesCXN: TfrUserDataset;
    CustomObservacaoFiscoCXN: TfrUserDataset;
    CustomISSQNCXN: TfrUserDataset;
    CustomEmitenteCXN: TfrUserDataset;
    CustomDestinatarioCXN: TfrUserDataset;
    CustomDadosProdutosCXN: TfrUserDataset;
    CustomParametrosCXN: TfrUserDataset;
    CustomInformacoesAdicionaisCXN: TfrUserDataset;
    CustomDuplicatasCXN: TfrUserDataset;
    CustomCalculoImpostoCXN: TfrUserDataset;
    CustomTransportadorCXN: TfrUserDataset;
  private
    { private declarations }
    FNFe: TNFe;
    FLogo: String;
  public
    { public declarations }
    procedure Imprimir(ANFe: TNFe; ALogo: String = '');
  end; 

var
  dmACBrNFe: TdmACBrNFe;

implementation

uses Math, StrUtils;

procedure TdmACBrNFe.Imprimir(ANFe: TNFe; ALogo: String);
begin
  FNFe  := ANFe;
  FLogo := ALogo;
  frReport1.LoadFromFile(ExtractFileDir(application.ExeName)+'\Report\NotaFiscalEletronica.lrf');
  frReport1.ShowReport
end;

initialization


end.

