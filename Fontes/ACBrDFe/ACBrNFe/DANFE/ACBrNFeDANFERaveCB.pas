{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                          }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 16/12/2008: Wemerson Souto
|*  - Doação do componente para o Projeto ACBr
|* 20/08/2009: João Paulo
|*  - Doação units para geração do Danfe via código usando Rave
******************************************************************************}
{$I ACBr.inc}
unit ACBrNFeDANFERaveCB;

interface

uses Forms, SysUtils, Classes, Graphics,
  RpDefine, RpDevice, RVClass, RVProj, RVCsBars, RVCsStd, RVCsData,
  RvDirectDataView, RVDataField,
  {$IFNDEF COMPILER16} JPEG, {$ELSE} Vcl.Imaging.jpeg, {$ENDIF}
  ACBrNFeDANFEClass, ACBrDANFeCBRave, pcnNFe, pcnConversao;

type
  TFont=(ftTimes,ftCourier);

  TACBrNFeDANFERaveCB = class( TACBrNFeDANFEClass )
   private
     FTamanhoCampoCodigo: integer;
     FTamanhoFonte_ANTT: integer;
     FTamanhoFonte_infComplementares: integer; 
     FFonte : TFont;
     FEspessuraBorda: Integer;
     FMostrarSetup: boolean;
     FTributosFonte: string;
     FTributosPercentual: TpcnPercentualTributos;
     FMarcaDaguaMSG: string;
     FTamanhoCampoVlUnit: integer;
     FExpandirDadosAdicionaisAuto: boolean;
     FimprimirDesconto: Boolean;
    FImprimirTributosItem: Boolean; 

     function SeSenaoJPEG(ACondicao: Boolean; ATrue, AFalse: TJPEGImage): TJPEGImage;
     function BMPtoJPGString(aBMPFile: string): string;
     function CarregaLogoMarca(LogoMarcaEmpresa: TJPEGImage): boolean;
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDANFE(NFE : TNFe = nil); override ;
    procedure ImprimirDANFEPDF(NFE : TNFe = nil); override ;
    procedure ImprimirEVENTO(NFE : TNFe = nil); override ;
    procedure ImprimirEVENTOPDF(NFE : TNFe = nil); override ;
    procedure ImprimirINUTILIZACAO(NFE : TNFe = nil); override ;
    procedure ImprimirINUTILIZACAOPDF(NFE : TNFe = nil); override ;
  published
     property TamanhoCampoCodigo:integer read FTamanhoCampoCodigo write FTamanhoCampoCodigo;
     property TamanhoCampoVlUnit:integer read FTamanhoCampoVlUnit write FTamanhoCampoVlUnit;
     property TamanhoFonte_ANTT:integer read FTamanhoFonte_ANTT write FTamanhoFonte_ANTT;
     property TamanhoFonte_infComplementares:integer read FTamanhoFonte_infComplementares write FTamanhoFonte_infComplementares;
     property Fonte:TFont read FFonte write FFonte;
     property EspessuraBorda:Integer read FEspessuraBorda write FEspessuraBorda;
     property ExpandirDadosAdicionaisAuto: boolean read FExpandirDadosAdicionaisAuto write FExpandirDadosAdicionaisAuto;
     property MostrarSetup: boolean read FMostrarSetup write FMostrarSetup;
     property TributosFonte: string read FTributosFonte write FTributosFonte;
     property TributosPercentual: TpcnPercentualTributos read FTributosPercentual write FTributosPercentual;
     property MarcaDaguaMSG: string read FMarcaDaguaMSG write FMarcaDaguaMSG;
     property ImprimirDesconto: Boolean read FimprimirDesconto write FImprimirDesconto; // #consult atech
     property ImprimirTributosItem: Boolean read FImprimirTributosItem write FImprimirTributosItem;
  end;

implementation

uses ACBrNFe, ACBrNFeUtil, ACBrUtil, ACBrDFeUtil, StrUtils, Dialogs;

function TACBrNFeDANFERaveCB.BMPtoJPGString(aBMPFile: string): string;
var
  aBmp: TBitmap;
  aJpg: TJpegImage;
  aStr: TStringStream;
begin
  Result:='';
  aBmp := TBitmap.Create;
  try
    aBmp.LoadFromFile(aBMPFile) ;
    aJpg := TJpegImage.Create;
    aStr := TStringStream.Create('');
    try
      aJpg.Assign(aBmp) ;
      aJpg.SaveToStream(aStr);
      Result := aStr.DataString;
    finally
      aStr.Free;
      aJpg.Free
    end;
  finally
    aBmp.Free
  end;
end;

function TACBrNFeDANFERaveCB.CarregaLogoMarca(
  LogoMarcaEmpresa: TJPEGImage): boolean;
var
  lExt: string;
  vStringStream: TStringStream;
begin
  Result := True;

  if DFeUtil.NaoEstaVazio(Logo) then
  begin
    if FileExists(Logo) then
    begin
      lExt := ExtractFileExt(Logo);
      if UpperCase(lExt) = '.JPG' then
        LogoMarcaEmpresa.LoadFromFile(Logo)
      else if UpperCase(lExt) = '.BMP' then
      begin
        vStringStream:= TStringStream.Create(BMPtoJPGString(Logo));
        try
          LogoMarcaEmpresa.LoadFromStream(vStringStream);
        finally
          vStringStream.Free;
        end;
      end
      else
        raise Exception.Create('Logo não é uma imagem JPG ou BMP válida.');
    end
    else
    begin
      vStringStream:= TStringStream.Create(Logo);
      try
        LogoMarcaEmpresa.LoadFromStream(vStringStream);
      finally
        vStringStream.Free;
      end;
    end;
  end
  else
    Result := False;
end;

constructor TACBrNFeDANFERaveCB.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FTamanhoCampoCodigo:=0;
  FTamanhoCampoVlUnit:=0;
  FTamanhoFonte_ANTT:=10;
  TamanhoFonte_infComplementares:=6; 
  FEspessuraBorda:=2;
  FMostrarSetup:=False;
  FTributosPercentual:=ptValorProdutos;
  FExpandirDadosAdicionaisAuto := False;
  FImprimirDesconto := True;
  FImprimirTributosItem := False;
end;

destructor TACBrNFeDANFERaveCB.Destroy;
begin
  inherited Destroy ;
end;


procedure TACBrNFeDANFERaveCB.ImprimirDANFE(NFE : TNFe = nil);
var
 LogoMarcaEmpresa:TJPEGImage;
 ExisteLogoMarca: Boolean;
begin
  LogoMarcaEmpresa:=TJPEGImage.Create;
  try
    ExisteLogoMarca := CarregaLogoMarca(LogoMarcaEmpresa);

    ImprimirDANFeRave(TACBrNFe(ACBrNFe),
                      Site,
                      Email,
                      Fax,
                      Sistema,
                      Usuario,
                      ProtocoloNFe,
                      SeSenaoJPEG(ExisteLogoMarca,LogoMarcaEmpresa,nil),
                      DFeUtil.SeSenao((TipoDANFE=tiRetrato),poPortrait,poLandScape),
                      DFeUtil.SeSenao(MostrarPreview,tsPreview,tsPrint),
                      MostrarStatus,
                      MostrarSetup,
                      NumCopias,
                      Impressora,
                      '',
                      MargemInferior*10,
                      MargemSuperior*10,
                      MargemEsquerda*10,
                      MargemDireita*10,
                      CasasDecimais._qCom,
                      CasasDecimais._vUnCom,
                      CasasDecimais._Mask_qCom,
                      CasasDecimais._Mask_vUnCom,
                      TamanhoCampoCodigo,
                      TamanhoCampoVlUnit,
                      TamanhoFonte_DemaisCampos,
                      TamanhoFonte_ANTT,
                      TamanhoFonte_infComplementares,
                      ProdutosPorPagina,
                      EspessuraBorda,
                      ExibirResumoCanhoto,
                      ExibirResumoCanhoto_Texto,
                      ImprimirDescPorc,
                      ImprimirDesconto, 
                      ImprimirTotalLiquido,
                      ImprimirDetalhamentoEspecifico,
                      ImprimirTributosItem,
                      FormularioContinuo,
                      ExpandirLogoMarca,
                      NFeCancelada,
                      TributosFonte,
                      TributosPercentual,
                      MarcaDaguaMSG,
                      LocalImpCanhoto,
                      ExpandirDadosAdicionaisAuto,
                      NFe);
  finally
    LogoMarcaEmpresa.Free;
  end;
end;

procedure TACBrNFeDANFERaveCB.ImprimirDANFEPDF(NFE : TNFe = nil);
var
 LogoMarcaEmpresa:TJPEGImage;
 ExisteLogoMarca: Boolean;
 NomeArq : String;
begin
    LogoMarcaEmpresa:=TJPEGImage.Create;
    try
      ExisteLogoMarca := CarregaLogoMarca(LogoMarcaEmpresa);

      if NFE = nil then
         NomeArq := StringReplace(TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFe.infNFe.ID,'NFe', '', [rfIgnoreCase])
      else
         NomeArq := StringReplace(NFE.infNFe.ID,'NFe', '', [rfIgnoreCase]);

      NomeArq := PathWithDelim(Self.PathPDF)+NomeArq+'-nfe.pdf';

      ImprimirDANFeRave(TACBrNFe(ACBrNFe),
                       Site,
                       Email,
                       Fax,
                       Sistema,
                       Usuario,
                       ProtocoloNFe,
                       SeSenaoJPEG(ExisteLogoMarca,LogoMarcaEmpresa,nil),
                       DFeUtil.SeSenao((TipoDANFE=tiRetrato),poPortrait,poLandScape),
                       tsPDF,
                       MostrarStatus,
                       MostrarSetup,
                       NumCopias,
                       Impressora,
                       NomeArq,
                       MargemInferior*10,
                       MargemSuperior*10,
                       MargemEsquerda*10,
                       MargemDireita*10,
                       CasasDecimais._qCom,
                       CasasDecimais._vUnCom,
                       CasasDecimais._Mask_qCom,
                       CasasDecimais._Mask_vUnCom,
                       TamanhoCampoCodigo,
                       TamanhoCampoVlUnit,
                       TamanhoFonte_DemaisCampos,
                       TamanhoFonte_ANTT,
                       TamanhoFonte_infComplementares,
                       ProdutosPorPagina,
                       EspessuraBorda,
                       ExibirResumoCanhoto,
                       ExibirResumoCanhoto_Texto,
                       ImprimirDescPorc,
                       ImprimirDesconto, 
                       ImprimirTotalLiquido,
                       ImprimirDetalhamentoEspecifico,
                       ImprimirTributosItem,
                       FormularioContinuo,
                       ExpandirLogoMarca,
                       NFeCancelada,
                       TributosFonte,
                       TributosPercentual,
                       MarcaDaguaMSG,
                       LocalImpCanhoto,
                       ExpandirDadosAdicionaisAuto,
                       NFE);
    finally
      LogoMarcaEmpresa.Free;
    end;
end;


procedure TACBrNFeDANFERaveCB.ImprimirEVENTO(NFE: TNFe);
var
 LogoMarcaEmpresa:TJPEGImage;
 ExisteLogoMarca: Boolean;
begin
    LogoMarcaEmpresa:=TJPEGImage.Create;
    try
      ExisteLogoMarca := CarregaLogoMarca(LogoMarcaEmpresa);

      ImprimirEventoRave(TACBrNFe(ACBrNFe),
                       Site,
                       Email,
                       Fax,
                       Sistema,
                       Usuario,
                       SeSenaoJPEG(ExisteLogoMarca,LogoMarcaEmpresa,nil),
                       DFeUtil.SeSenao((TipoDANFE=tiRetrato),poPortrait,poLandScape),
                       DFeUtil.SeSenao(MostrarPreview,tsPreview,tsPrint),
                       MostrarStatus,
                       MostrarSetup,
                       NumCopias,
                       Impressora,
                       '',
                       MargemInferior*10,
                       MargemSuperior*10,
                       MargemEsquerda*10,
                       MargemDireita*10,
                       TamanhoFonte_DemaisCampos,
                       EspessuraBorda,
                       FormularioContinuo,
                       ExpandirLogoMarca,
                       NFe);
    finally
      LogoMarcaEmpresa.Free;
    end;
end;


procedure TACBrNFeDANFERaveCB.ImprimirEVENTOPDF(NFE: TNFe);
var
 LogoMarcaEmpresa:TJPEGImage;
 ExisteLogoMarca: Boolean;
 NomeArq : String;
begin
    LogoMarcaEmpresa:=TJPEGImage.Create;
    try
      ExisteLogoMarca := CarregaLogoMarca(LogoMarcaEmpresa);

      NomeArq := StringReplace(TACBrNFe(ACBrNFe).EventoNFe.Evento[0].InfEvento.id,'ID', '', [rfIgnoreCase]);
      NomeArq := PathWithDelim(Self.PathPDF)+NomeArq+'-procEventoNFe.pdf';

      ImprimirEventoRave(TACBrNFe(ACBrNFe),
                       Site,
                       Email,
                       Fax,
                       Sistema,
                       Usuario,
                       SeSenaoJPEG(ExisteLogoMarca,LogoMarcaEmpresa,nil),
                       DFeUtil.SeSenao((TipoDANFE=tiRetrato),poPortrait,poLandScape),
                       tsPDF,
                       MostrarStatus,
                       MostrarSetup,
                       NumCopias,
                       Impressora,
                       NomeArq,
                       MargemInferior*10,
                       MargemSuperior*10,
                       MargemEsquerda*10,
                       MargemDireita*10,
                       TamanhoFonte_DemaisCampos,
                       EspessuraBorda,
                       FormularioContinuo,
                       ExpandirLogoMarca,
                       NFe);
    finally
      LogoMarcaEmpresa.Free;
    end;
end;

procedure TACBrNFeDANFERaveCB.ImprimirINUTILIZACAO(NFE: TNFe);
var
 LogoMarcaEmpresa:TJPEGImage;
 ExisteLogoMarca: Boolean;
begin
    LogoMarcaEmpresa:=TJPEGImage.Create;
    try
      ExisteLogoMarca := CarregaLogoMarca(LogoMarcaEmpresa);

      ImprimirInutilizacaoRave(TACBrNFe(ACBrNFe),
                       Site,
                       Email,
                       Fax,
                       Sistema,
                       Usuario,
                       SeSenaoJPEG(ExisteLogoMarca,LogoMarcaEmpresa,nil),
                       DFeUtil.SeSenao((TipoDANFE=tiRetrato),poPortrait,poLandScape),
                       DFeUtil.SeSenao(MostrarPreview,tsPreview,tsPrint),
                       MostrarStatus,
                       MostrarSetup,
                       NumCopias,
                       Impressora,
                       '',
                       MargemInferior*10,
                       MargemSuperior*10,
                       MargemEsquerda*10,
                       MargemDireita*10,
                       TamanhoFonte_DemaisCampos,
                       EspessuraBorda,
                       FormularioContinuo,
                       ExpandirLogoMarca,
                       NFe);
    finally
      LogoMarcaEmpresa.Free;
    end;
end;

procedure TACBrNFeDANFERaveCB.ImprimirINUTILIZACAOPDF(NFE: TNFe);
var
 LogoMarcaEmpresa:TJPEGImage;
 ExisteLogoMarca: Boolean;
 NomeArq : String;
begin
    LogoMarcaEmpresa:=TJPEGImage.Create;
    try
      ExisteLogoMarca := CarregaLogoMarca(LogoMarcaEmpresa);

      NomeArq := StringReplace(TACBrNFe(ACBrNFe).InutNFe.ID,'ID', '', [rfIgnoreCase]);
      NomeArq := PathWithDelim(Self.PathPDF)+NomeArq+'-procInutNFe.pdf';

      ImprimirInutilizacaoRave(TACBrNFe(ACBrNFe),
                       Site,
                       Email,
                       Fax,
                       Sistema,
                       Usuario,
                       SeSenaoJPEG(ExisteLogoMarca,LogoMarcaEmpresa,nil),
                       DFeUtil.SeSenao((TipoDANFE=tiRetrato),poPortrait,poLandScape),
                       tsPDF,
                       MostrarStatus,
                       MostrarSetup,
                       NumCopias,
                       Impressora,
                       NomeArq,
                       MargemInferior*10,
                       MargemSuperior*10,
                       MargemEsquerda*10,
                       MargemDireita*10,
                       TamanhoFonte_DemaisCampos,
                       EspessuraBorda,
                       FormularioContinuo,
                       ExpandirLogoMarca,
                       NFe);
    finally
      LogoMarcaEmpresa.Free;
    end;
end;

function TACBrNFeDANFERaveCB.SeSenaoJPEG(ACondicao: Boolean; ATrue,
  AFalse: TJPEGImage): TJPEGImage;
begin
  Result := AFalse;
  if ACondicao then
    Result := ATrue;
end;

end.
