{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFeDAMDFeClass;

interface

uses
  SysUtils, Classes,
  ACBrDFeReport,
  pmdfeMDFe, pcnConversao;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrMDFeDAMDFeClass = class(TACBrDFeReport)
   private
    procedure SetMDFe(const Value: TComponent);
    procedure ErroAbstract(NomeProcedure: String);

  protected
    FACBrMDFe: TComponent;
    FImprimirHoraSaida: Boolean;
    FImprimirHoraSaida_Hora: String;
    FTipoDAMDFe: TpcnTipoImpressao;
    FTamanhoPapel: TpcnTamanhoPapel;
    FProtocoloMDFe: String;
    FMDFeCancelada: Boolean;
    FMDFeEncerrado: Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetSeparadorPathPDF: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDAMDFe(MDFe: TMDFe = nil); virtual;
    procedure ImprimirDAMDFePDF(MDFe: TMDFe = nil); virtual;
    procedure ImprimirEVENTO(MDFe: TMDFe = nil); virtual;
    procedure ImprimirEVENTOPDF(MDFe: TMDFe = nil); virtual;
  published
    property ACBrMDFe: TComponent           read FACBrMDFe               write SetMDFe;
    property ImprimeHoraSaida: Boolean      read FImprimirHoraSaida      write FImprimirHoraSaida;
    property ImprimeHoraSaida_Hora: String  read FImprimirHoraSaida_Hora write FImprimirHoraSaida_Hora;
    property TipoDAMDFe: TpcnTipoImpressao  read FTipoDAMDFe             write FTipoDAMDFe;
    property TamanhoPapel: TpcnTamanhoPapel read FTamanhoPapel           write FTamanhoPapel;
    property Protocolo: String              read FProtocoloMDFe          write FProtocoloMDFe;
    property Cancelada: Boolean             read FMDFeCancelada          write FMDFeCancelada;
    property Encerrado: Boolean             read FMDFeEncerrado          write FMDFeEncerrado;
  end;

implementation

uses
  ACBrMDFe, ACBrUtil;

constructor TACBrMDFeDAMDFeClass.Create(AOwner: TComponent);
begin
  inherited create(AOwner);

  FACBrMDFe   := nil;
  FImprimirHoraSaida      := False;
  FImprimirHoraSaida_Hora := '';
  FProtocoloMDFe := '';
  FMDFeCancelada  := False;
  FMDFeEncerrado  := False;
end;

destructor TACBrMDFeDAMDFeClass.Destroy;
begin

  inherited Destroy;
end;

procedure TACBrMDFeDAMDFeClass.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrMDFe <> nil) and (AComponent is TACBrMDFe) then
     FACBrMDFe := nil;
end;

procedure TACBrMDFeDAMDFeClass.SetMDFe(const Value: TComponent);
var
  OldValue: TACBrMDFe;
begin
  if Value <> FACBrMDFe then
  begin
     if Value <> nil then
        if not (Value is TACBrMDFe) then
           raise Exception.Create('DAMDFe deve ser do tipo TACBrMDFe');

     if Assigned(FACBrMDFe) then
        FACBrMDFe.RemoveFreeNotification(Self);

     OldValue := TACBrMDFe(FACBrMDFe);   // Usa outra variavel para evitar Loop Infinito
     FACBrMDFe := Value;                 // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.DAMDFe) then
           OldValue.DAMDFe := nil;

     if Value <> nil then
     begin
        Value.FreeNotification(self);
        TACBrMDFe(Value).DAMDFe := self;
     end;
  end;
end;

procedure TACBrMDFeDAMDFeClass.ErroAbstract(NomeProcedure: String);
begin
  raise Exception.Create(NomeProcedure);
end;

procedure TACBrMDFeDAMDFeClass.ImprimirDAMDFe(MDFe: TMDFe = nil);
begin
  ErroAbstract('ImprimirDAMDFe');
end;

procedure TACBrMDFeDAMDFeClass.ImprimirDAMDFePDF(MDFe: TMDFe = nil);
begin
  ErroAbstract('ImprimirDAMDFePDF');
end;

procedure TACBrMDFeDAMDFeClass.ImprimirEVENTO(MDFe: TMDFe);
begin
  ErroAbstract('ImprimirEVENTO');
end;

procedure TACBrMDFeDAMDFeClass.ImprimirEVENTOPDF(MDFe: TMDFe);
begin
  ErroAbstract('ImprimirEVENTOPDF');
end;

function TACBrMDFeDAMDFeClass.GetSeparadorPathPDF: String;
var
  dhEmissao: TDateTime;
  DescricaoModelo: String;
  AMDFe: TMDFe;
begin
  Result := '';

  if Assigned(ACBrMDFe) then  // Se tem o componente ACBrMDFe
  begin
     if TACBrMDFe(ACBrMDFe).Manifestos.Count > 0 then  // Se tem algum Manifesto carregado
     begin
       AMDFe := TACBrMDFe(ACBrMDFe).Manifestos.Items[0].MDFe;
       if TACBrMDFe(ACBrMDFe).Configuracoes.Arquivos.EmissaoPathMDFe then
         dhEmissao := AMDFe.Ide.dhEmi
       else
         dhEmissao := Now;

       DescricaoModelo := 'MDFe';
       Result := TACBrMDFe(FACBrMDFe).Configuracoes.Arquivos.GetPath(
                         Result,
                         DescricaoModelo,
                         AMDFe.Emit.CNPJCPF,
                         dhEmissao,
                         DescricaoModelo);
     end;
     Result := PathWithDelim(Result);
  end;
end;

end.
