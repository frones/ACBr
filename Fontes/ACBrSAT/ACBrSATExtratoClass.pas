{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 04/04/2013:  André Ferreira de Moraes
|*   Inicio do desenvolvimento
******************************************************************************}

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
     ,ACBrBase, ACBrConsts, pcnCFe, pcnCFeCanc;

const
  cMsgAppQRCode = 'Consulte o QR Code pelo aplicativo  "De olho na nota", '+
                  'disponível na AppStore (Apple) e PlayStore (Android)';

type
   TACBrSATExtratoFiltro = (fiNenhum, fiPDF, fiHTML ) ;

   TACBrSATExtratoLayOut = (lCompleto, lResumido, lCancelamento) ;


   { TACBrSATExtratoMargem }

   TACBrSATExtratoMargem = class( TPersistent )
   private
     fDireita: Integer;
     fEsquerda: Integer;
     fFundo: Integer;
     fTopo: Integer;
   public
     constructor create;
   published
     property Topo     : Integer read fTopo     write fTopo     default 2;
     property Esquerda : Integer read fEsquerda write fEsquerda default 2;
     property Fundo    : Integer read fFundo    write fFundo    default 4;
     property Direita  : Integer read fDireita  write fDireita  default 2;
   end;

  { TACBrSATExtratoClass }

  TACBrSATExtratoClass = class( TACBrComponent )
  private
    fACBrSAT : TComponent;
    fImprimeQRCode: Boolean;
    fCFe: TCFe;
    fCFeCanc: TCFeCanc;

    fFiltro: TACBrSATExtratoFiltro;
    fMask_qCom: String;
    fMask_vUnCom: String;
    fMostrarPreview: Boolean;
    fMostrarSetup: Boolean;
    fMsgAppQRCode: String;
    fNomeArquivo: String;
    fNumCopias: Integer;
    fPrinterName : String;
    {$IFNDEF NOGUI}
     fPictureLogo: {$IFDEF FMX}TBitmap{$ELSE}TPicture{$ENDIF};
    {$ENDIF}
    fSoftwareHouse: String;
    fSite: String;

    procedure ErroAbstract(NomeProcedure : String) ;
    function GetAbout: String;
    function GetNomeArquivo: String;
    procedure SetAbout(AValue: String);
    procedure SetNumCopias(AValue: Integer);
    {$IFNDEF NOGUI}
     procedure SetPictureLogo(AValue: {$IFDEF FMX}TBitmap{$ELSE}TPicture{$ENDIF});
    {$ENDIF}
    procedure SetSAT(const Value: TComponent);

    procedure SetInternalCFe(ACFe: TCFe);
    procedure SetInternalCFeCanc(ACFeCanc: TCFeCanc);
    procedure VerificaExisteACBrSAT;
  protected
    fpAbout : String ;
    fpLayOut: TACBrSATExtratoLayOut;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property PrinterName    : String   read fPrinterName    write fPrinterName;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property LayOut  : TACBrSATExtratoLayOut read fpLayOut ;
    property CFe     : TCFe                  read fCFe;
    property CFeCanc : TCFeCanc              read fCFeCanc;

    procedure ImprimirExtrato(ACFe : TCFe = nil); virtual;
    procedure ImprimirExtratoResumido(ACFe : TCFe = nil); virtual;
    procedure ImprimirExtratoCancelamento(ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); virtual;

    function CalcularConteudoQRCode(ID: String; dEmi_hEmi: TDateTime;
      Valor: Double; CNPJCPF: String; assinaturaQRCODE: String): String;
  published
    property ACBrSAT  : TComponent  read FACBrSAT write SetSAT ;

    property About  : String read GetAbout write SetAbout stored False ;

    property Mask_qCom      : String   read fMask_qCom      write fMask_qCom;
    property Mask_vUnCom    : String   read fMask_vUnCom    write fMask_vUnCom;
    property ImprimeQRCode  : Boolean  read fImprimeQRCode  write fImprimeQRCode  default True ;
    {$IFNDEF NOGUI}
     property PictureLogo    : {$IFDEF FMX}TBitmap{$ELSE}TPicture{$ENDIF} read fPictureLogo    write SetPictureLogo ;
    {$ENDIF}
    property MostrarPreview : Boolean  read fMostrarPreview write fMostrarPreview default False ;
    property MostrarSetup   : Boolean  read fMostrarSetup   write fMostrarSetup   default False ;
    property NumCopias      : Integer  read fNumCopias      write SetNumCopias    default 1;
    property NomeArquivo    : String   read GetNomeArquivo  write fNomeArquivo ;
    property SoftwareHouse  : String   read fSoftwareHouse  write fSoftwareHouse;
    property Site           : String   read fSite           write fSite;
    property Filtro         : TACBrSATExtratoFiltro read fFiltro write fFiltro default fiNenhum ;
    property MsgAppQRCode   : String   read fMsgAppQRCode   write fMsgAppQRCode;
  end ;

implementation

uses ACBrSAT, ACBrSATClass, ACBrUtil;

{ TACBrSATExtratoMargem }

constructor TACBrSATExtratoMargem.create;
begin
  inherited create;

  fDireita  := 2;
  fEsquerda := 2;
  fTopo     := 2;
  fFundo    := 4;
end;

{ TACBrSATExtratoClass }

constructor TACBrSATExtratoClass.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  fpAbout  := 'ACBrSATExtratoClass' ;
  fpLayOut := lCompleto;

  fACBrSAT := nil;
  fCFe     := nil;
  fCFeCanc := nil;

  {$IFNDEF NOGUI}
   fPictureLogo := {$IFDEF FMX}TBitmap{$ELSE}TPicture{$ENDIF}.Create;
  {$ENDIF}

  fNumCopias      := 1;
  fMostrarPreview := False;
  fMostrarSetup   := False;
  fImprimeQRCode  := True;
  fFiltro         := fiNenhum;
  fNomeArquivo    := '' ;
  fMask_qCom      := ',0.0000';
  fMask_vUnCom    := ',0.000';
  fPrinterName    := '' ;
  fSite           := 'http://www.projetoacbr.com.br';
  fSoftwareHouse  := 'Projeto ACBr';
  fMsgAppQRCode   := ACBrStr(cMsgAppQRCode);
end;

destructor TACBrSATExtratoClass.Destroy;
begin
  {$IFNDEF NOGUI}
   fPictureLogo.Free;
  {$ENDIF}

  inherited Destroy ;
end;

procedure TACBrSATExtratoClass.ImprimirExtrato(ACFe: TCFe);
begin
  SetInternalCFe( ACFe );
  fpLayOut := lCompleto;
end;

procedure TACBrSATExtratoClass.ImprimirExtratoCancelamento(ACFe: TCFe;
  ACFeCanc: TCFeCanc);
begin
  SetInternalCFe( ACFe );
  SetInternalCFeCanc( ACFeCanc );
  fpLayOut := lCancelamento;
end;

function TACBrSATExtratoClass.CalcularConteudoQRCode(ID: String;
  dEmi_hEmi:TDateTime; Valor: Double; CNPJCPF: String;
  assinaturaQRCODE: String): String;
begin
  Result := ID + '|' +
            FormatDateTime('yyyymmddhhmmss',dEmi_hEmi) + '|' +
            FloatToString(Valor,'.','0.00') + '|' +
            Trim(CNPJCPF) + '|' +
            assinaturaQRCODE;
end;

procedure TACBrSATExtratoClass.ImprimirExtratoResumido(ACFe: TCFe);
begin
  SetInternalCFe( ACFe );
  fpLayOut := lResumido;
end;

procedure TACBrSATExtratoClass.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrSAT <> nil) and (AComponent is TACBrSAT) then
     FACBrSAT := nil ;
end;

procedure TACBrSATExtratoClass.ErroAbstract(NomeProcedure : String) ;
begin
  raise EACBrSATErro.create( Format( 'Procedure: %s '+ sLineBreak +
                                     ' não implementada para o Extrato: %s' ,
                                     [NomeProcedure, ClassName] )) ;
end ;

function TACBrSATExtratoClass.GetAbout: String;
begin
  Result := fpAbout ;
end;

function TACBrSATExtratoClass.GetNomeArquivo: String;
var
  wPath: String;
begin
   wPath  := ExtractFilePath(fNomeArquivo);
   Result := '';

   if wPath = '' then
      if not (csDesigning in Self.ComponentState) then
         Result := ExtractFilePath(ParamStr(0)) ;

   if fNomeArquivo = '' then
     fNomeArquivo := TACBrSAT(FACBrSAT).CFe.infCFe.ID+'.pdf';

   Result := trim(Result + fNomeArquivo);
end;

procedure TACBrSATExtratoClass.SetAbout(AValue: String);
begin
  {}
end;

procedure TACBrSATExtratoClass.SetNumCopias(AValue: Integer);
begin
  if fNumCopias = AValue then Exit;
  fNumCopias := AValue;
end;

{$IFNDEF NOGUI}
procedure TACBrSATExtratoClass.SetPictureLogo(AValue: {$IFDEF FMX}TBitmap{$ELSE}TPicture{$ENDIF});
begin
  fPictureLogo.Assign( AValue );
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
    fCFe := TACBrSAT(ACBrSAT).CFe;
  end
  else
    fCFe := ACFe;
end;

procedure TACBrSATExtratoClass.SetInternalCFeCanc(ACFeCanc: TCFeCanc);
begin
  if ACFeCanc = nil then
  begin
    VerificaExisteACBrSAT;
    fCFeCanc := TACBrSAT(ACBrSAT).CFeCanc;
  end
  else
    fCFeCanc := ACFeCanc;
end;

procedure TACBrSATExtratoClass.VerificaExisteACBrSAT;
begin
  if not Assigned(ACBrSAT) then
     raise Exception.Create('Componente ACBrSAT não atribuído');
end;

end.
