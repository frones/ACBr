{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
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
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}

{$I ACBr.inc}

unit ACBrGNREGuiaClass;

interface

uses
  Forms, SysUtils, Classes, ACBrBase, pcnConversao, pgnreGNRERetorno;

type
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrGNREGuiaClass = class( TACBrComponent )
  private
    procedure SetGNRE(const Value: TComponent);
    procedure ErroAbstract( NomeProcedure : String );
    function GetPathPDF: String;
    procedure SetPathPDF(const Value: String);
  protected
    FACBrGNRE : TComponent;
    FSistema:String;
    FUsuario:String;
    FPathPDF : String;
    FImpressora : String;
    FMostrarPreview : Boolean;
    FMostrarStatus: Boolean;
    FTamanhoPapel: TpcnTamanhoPapel;
    FNumCopias : Integer;
    FFax  : String;
    FSite : String;
    FEmail: String;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirGuia(GNRE: TGNRERetorno = nil); virtual;
    procedure ImprimirGuiaPDF(GNRE: TGNRERetorno = nil); virtual;
  published
    property ACBrGNRE : TComponent  read FACBrGNRE write SetGNRE;
    property Sistema: String read FSistema write FSistema;
    property Usuario: String read FUsuario write FUsuario;
    property PathPDF: String read GetPathPDF write SetPathPDF;
    property Impressora: String read FImpressora write FImpressora;
    property MostrarPreview: Boolean read FMostrarPreview write FMostrarPreview;
    property MostrarStatus: Boolean read FMostrarStatus write FMostrarStatus;
    property TamanhoPapel: TpcnTamanhoPapel read FTamanhoPapel write FTamanhoPapel;
    property NumCopias: Integer read FNumCopias write FNumCopias;
    property Fax  : String read FFax   write FFax;
    property Site : String read FSite  write FSite;
    property Email: String read FEmail write FEmail;
    property MargemInferior: Double read FMargemInferior write FMargemInferior;
    property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
    property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
    property MargemDireita: Double read FMargemDireita write FMargemDireita;
  end;

implementation

uses
  ACBrGNRE2, ACBrUtil, ACBrDFeUtil;

constructor TACBrGNREGuiaClass.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FACBrGNRE   := nil;
  FSistema    := '';
  FUsuario    := '';
  FPathPDF    := '';
  FImpressora := '';

  FMostrarPreview := True;
  FMostrarStatus  := True;
  FNumCopias      := 1;

  FFax   := '';
  FSite  := '';
  FEmail := '';

  FMargemInferior := 0.8;
  FMargemSuperior := 0.8;
  FMargemEsquerda := 0.6;
  FMargemDireita  := 0.51;
end;

destructor TACBrGNREGuiaClass.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrGNREGuiaClass.ImprimirGuia(GNRE: TGNRERetorno = nil);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrGNREGuiaClass.ImprimirGuiaPDF(GNRE: TGNRERetorno = nil);
begin
  ErroAbstract('ImprimirPDF');
end;

procedure TACBrGNREGuiaClass.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrGNRE <> nil) and (AComponent is TACBrGNRE) then
    FACBrGNRE := nil;
end;

procedure TACBrGNREGuiaClass.SetGNRE(const Value: TComponent);
var
  OldValue: TACBrGNRE;
begin
  if Value <> FACBrGNRE then
  begin
    if Value <> nil then
      if not (Value is TACBrGNRE) then
        raise Exception.Create('ACBrGNREGuia.GNRE deve ser do tipo TACBrGNRE');

    if Assigned(FACBrGNRE) then
      FACBrGNRE.RemoveFreeNotification(Self);

    OldValue := TACBrGNRE(FACBrGNRE);   // Usa outra variavel para evitar Loop Infinito
    FACBrGNRE := Value;                 // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.GNREGuia) then
        OldValue.GNREGuia := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(Self);
      TACBrGNRE(Value).GNREGuia := Self;
    end;
  end;
end;

procedure TACBrGNREGuiaClass.ErroAbstract(NomeProcedure: String);
begin
  raise Exception.Create( NomeProcedure );
end;

function TACBrGNREGuiaClass.GetPathPDF: String;
var
  aPath: string;
begin
  if (csDesigning in ComponentState) then
  begin
    Result := FPathPDF;
    Exit;
  end;

  aPath := Trim(FPathPDF);

  if EstaVazio(aPath) then  // Se não pode definir o Parth, use o Path da Aplicaçao
    aPath := PathWithDelim( ExtractFilePath(ParamStr(0))) + 'pdf';

  if NaoEstaVazio(aPath) then
    if not DirectoryExists(aPath) then
      ForceDirectories(aPath);

  Result := PathWithDelim( aPath );
  {
  if EstaVazio(FPathPDF) then
    if Assigned(FACBrGNRE) then
      FPathPDF := TACBrGNRE(FACBrGNRE).Configuracoes.Arquivos.PathSalvar;

  if NaoEstaVazio(FPathPDF) then
    if not DirectoryExists(FPathPDF) then
      ForceDirectories(FPathPDF);

  Result := PathWithDelim(FPathPDF);
  }
end;


procedure TACBrGNREGuiaClass.SetPathPDF(const Value: String);
begin
  FPathPDF := PathWithDelim(Value);
end;
end.
