{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: André Ferreira de Moraes                        }
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

unit ACBrSATExtratoClass;

interface

uses SysUtils,
     Classes
     {$IFNDEF NOGUI}
      {$IF DEFINED(VisualCLX)}
         ,QGraphics
      {$ELSEIF DEFINED(FMX)}
         ,FMX.Graphics
      {$ELSE}
         ,Graphics
      {$IFEND}
     {$ENDIF}
     ,ACBrBase, ACBrConsts, pcnCFe, pcnCFeCanc,
     ACBrDFeReport;

const
  cMsgAppQRCodeSP = 'Consulte o QR Code pelo aplicativo  "De olho na nota", '+
                    'disponível na AppStore (Apple) e PlayStore (Android)';

  cMsgAppQRCodeCE = 'Consulte o QR Code pelo aplicativo  "Sua Nota Tem Valor", '+
                    'disponível na AppStore (Apple) e PlayStore (Android)';
type
   TACBrSATExtratoFiltro = (fiNenhum, fiPDF, fiHTML ) ;

   TACBrSATExtratoLayOut = (lCompleto, lResumido, lCancelamento) ;

  { TACBrSATExtratoClass }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrSATExtratoClass = class( TACBrDFeReport )
  private
    FACBrSAT : TComponent;
    FImprimeQRCode: Boolean;
    FImprimeMsgOlhoNoImposto : Boolean;
    FImprimeCPFNaoInformado : Boolean;
    FImprimeQRCodeLateral: Boolean;
    FImprimeLogoLateral: Boolean;

    FCFe: TCFe;
    FCFeCanc: TCFeCanc;

    FFiltro: TACBrSATExtratoFiltro;
    FMsgAppQRCode: String;
    {$IFNDEF NOGUI}
     FPictureLogo: {$IFDEF FMX}TBitmap{$ELSE}TPicture{$ENDIF};
    {$ENDIF}
    FImprimeDescAcrescItem: Boolean;
    FImprimeEmUmaLinha: Boolean;
    FImprimeCodigoEan: Boolean;


    procedure ErroAbstract(const NomeProcedure : String) ;
    {$IFNDEF NOGUI}
     procedure SetPictureLogo(AValue: {$IFDEF FMX}TBitmap{$ELSE}TPicture{$ENDIF});
    {$ENDIF}
    procedure SetSAT(const Value: TComponent);

    procedure SetInternalCFeCanc(ACFeCanc: TCFeCanc);
    procedure VerificaExisteACBrSAT;
  protected
    FLayOut: TACBrSATExtratoLayOut;
    FStream: TStream;

    function GetSeparadorPathPDF(const aInitialPath: String): String; override;
    procedure SetInternalCFe(ACFe: TCFe);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property LayOut  : TACBrSATExtratoLayOut read FLayOut ;
    property CFe     : TCFe                  read FCFe;
    property CFeCanc : TCFeCanc              read FCFeCanc;

    procedure ImprimirExtrato(ACFe : TCFe = nil); overload; virtual;
    procedure ImprimirExtratoResumido(ACFe : TCFe = nil); overload; virtual;
    procedure ImprimirExtratoCancelamento(ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); overload; virtual;
    procedure ImprimirExtrato(AStream: TStream; ACFe : TCFe = nil); overload; virtual;
    procedure ImprimirExtratoResumido(AStream: TStream; ACFe : TCFe = nil); overload; virtual;
    procedure ImprimirExtratoCancelamento(AStream: TStream; ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); overload; virtual;

    function CalcularConteudoQRCode(const ID: String; dEmi_hEmi: TDateTime;
      Valor: Double; const CNPJCPF: String; const assinaturaQRCODE: String): String;
  published
    property ACBrSAT  : TComponent  read FACBrSAT write SetSAT ;
    property ImprimeQRCode  : Boolean  read FImprimeQRCode  write FImprimeQRCode  default True ;
    property ImprimeMsgOlhoNoImposto : Boolean read FImprimeMsgOlhoNoImposto write FImprimeMsgOlhoNoImposto default True;
    property ImprimeCPFNaoInformado : Boolean read FImprimeCPFNaoInformado write FImprimeCPFNaoInformado default True;
    {$IFNDEF NOGUI}
    property PictureLogo: {$IFDEF FMX}TBitmap{$ELSE}TPicture{$ENDIF} read FPictureLogo    write SetPictureLogo ;
    {$ENDIF}
    property Filtro: TACBrSATExtratoFiltro read FFiltro write FFiltro default fiNenhum ;
    property MsgAppQRCode: String   read FMsgAppQRCode   write FMsgAppQRCode;
    property ImprimeEmUmaLinha: Boolean     read FImprimeEmUmaLinha     write FImprimeEmUmaLinha     default True;
    property ImprimeDescAcrescItem: Boolean read FImprimeDescAcrescItem write FImprimeDescAcrescItem default True;
    property ImprimeCodigoEan: Boolean read FImprimeCodigoEan write FImprimeCodigoEan default False;
    property ImprimeQRCodeLateral: Boolean read FImprimeQRCodeLateral write FImprimeQRCodeLateral default True;
    property ImprimeLogoLateral: Boolean read FImprimeLogoLateral write FImprimeLogoLateral default True;
    property FormularioContinuo;
  end;

implementation

uses ACBrSAT, ACBrSATClass, ACBrUtil.Strings, ACBrUtil.Base, ACBrUtil.FilesIO,
  StrUtils;

{ TACBrSATExtratoClass }
constructor TACBrSATExtratoClass.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FLayOut := lCompleto;

  FACBrSAT := nil;
  FCFe     := nil;
  FCFeCanc := nil;

  {$IFNDEF NOGUI}
   FPictureLogo := {$IFDEF FMX}TBitmap{$ELSE}TPicture{$ENDIF}.Create;
  {$ENDIF}
  FImprimeQRCode  := True;
  FFiltro         := fiNenhum;

  FImprimeMsgOlhoNoImposto := True;
  FImprimeCPFNaoInformado  := True;
  FImprimeEmUmaLinha       := True;
  FImprimeDescAcrescItem   := True;
  FImprimeCodigoEan        := False;
  FImprimeQRCodeLateral    := True;
  FImprimeLogoLateral      := True;

  FormularioContinuo := True;
end;

destructor TACBrSATExtratoClass.Destroy;
begin
  {$IFNDEF NOGUI}
   FPictureLogo.Free;
  {$ENDIF}

  inherited Destroy ;
end;

procedure TACBrSATExtratoClass.ImprimirExtrato(ACFe: TCFe);
begin
  SetInternalCFe(ACFe);
  FLayOut := lCompleto;
end;

procedure TACBrSATExtratoClass.ImprimirExtratoCancelamento(ACFe: TCFe; ACFeCanc: TCFeCanc);
begin
  SetInternalCFe(ACFe);
  SetInternalCFeCanc(ACFeCanc);
  FLayOut := lCancelamento;
end;

procedure TACBrSATExtratoClass.ImprimirExtratoResumido(ACFe: TCFe);
begin
  SetInternalCFe( ACFe );
  FLayOut := lResumido;
end;

procedure TACBrSATExtratoClass.ImprimirExtrato(AStream: TStream; ACFe : TCFe = nil);
begin
  SetInternalCFe(ACFe);
  FLayOut := lCompleto;
  FStream := AStream;
end;

procedure TACBrSATExtratoClass.ImprimirExtratoCancelamento(AStream: TStream; ACFe: TCFe; ACFeCanc: TCFeCanc);
begin
  SetInternalCFe(ACFe);
  SetInternalCFeCanc(ACFeCanc);
  FLayOut := lCancelamento;
  FStream := AStream;
end;

procedure TACBrSATExtratoClass.ImprimirExtratoResumido(AStream: TStream; ACFe: TCFe);
begin
  SetInternalCFe( ACFe );
  FLayOut := lResumido;
  FStream := AStream;
end;

function TACBrSATExtratoClass.CalcularConteudoQRCode(const ID: String; dEmi_hEmi:TDateTime; Valor: Double;
  const CNPJCPF: String; const assinaturaQRCODE: String): String;
begin
  Result := ID + '|' +
            FormatDateTime('yyyymmddhhnnss',dEmi_hEmi) + '|' +
            FloatToString(Valor,'.','0.00') + '|' +
            Trim(CNPJCPF) + '|' +
            assinaturaQRCODE;
end;

procedure TACBrSATExtratoClass.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrSAT <> nil) and (AComponent is TACBrSAT) then
     FACBrSAT := nil ;
end;

procedure TACBrSATExtratoClass.ErroAbstract(const NomeProcedure : String) ;
begin
  raise EACBrSATErro.create( Format( 'Procedure: %s '+ sLineBreak +
                                     ' não implementada para o Extrato: %s' ,
                                     [NomeProcedure, ClassName] )) ;
end ;

function TACBrSATExtratoClass.GetSeparadorPathPDF(const aInitialPath: String): String;
begin
   Result := PathWithDelim(aInitialPath) + 'SAT';
end;

{$IFNDEF NOGUI}
procedure TACBrSATExtratoClass.SetPictureLogo(AValue: {$IFDEF FMX}TBitmap{$ELSE}TPicture{$ENDIF});
begin
  FPictureLogo.Assign( AValue );
end;
{$ENDIF}

procedure TACBrSATExtratoClass.SetSAT(const Value: TComponent);
var
  OldValue : TACBrSAT ;
begin
  if Value <> FACBrSAT then
  begin
     if Value <> nil then
        if not (Value is TACBrSAT) then
           raise Exception.Create('ACBrSATExtrato.SAT deve ser do tipo TACBrSAT') ;

     if Assigned(FACBrSAT) then
        FACBrSAT.RemoveFreeNotification(Self);

     OldValue := TACBrSAT(FACBrSAT) ;   // Usa outra variavel para evitar Loop Infinito
     FACBrSAT := Value;                 // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.Extrato) then
           OldValue.Extrato := nil ;

     if Value <> nil then
     begin
        Value.FreeNotification(self);
        TACBrSAT(Value).Extrato := self ;
     end ;
  end ;
end;

procedure TACBrSATExtratoClass.SetInternalCFe(ACFe: TCFe);
begin
  if ACFe = nil then
  begin
    VerificaExisteACBrSAT;
    FCFe := TACBrSAT(ACBrSAT).CFe;
  end
  else
    FCFe := ACFe;

  if TACBrSAT(ACBrSAT).Extrato.MsgAppQRCode = '' then
    FMsgAppQRCode := ACBrStr(ifThen(FCFe.ide.cUF = 35,cMsgAppQRCodeSP,cMsgAppQRCodeCE));
end;

procedure TACBrSATExtratoClass.SetInternalCFeCanc(ACFeCanc: TCFeCanc);
begin
  if ACFeCanc = nil then
  begin
    VerificaExisteACBrSAT;
    FCFeCanc := TACBrSAT(ACBrSAT).CFeCanc;
  end
  else
    FCFeCanc := ACFeCanc;
end;

procedure TACBrSATExtratoClass.VerificaExisteACBrSAT;
begin
  if not Assigned(ACBrSAT) then
     raise Exception.Create('Componente ACBrSAT não atribuído');
end;

end.
