{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - CTe - http://www.cte.fazenda.gov.br            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Wiliam Zacarias da Silva Rosa          }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Desenvolvimento                                                              }
{         de Cte: Wiliam Zacarias da Silva Rosa                                }
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
{ Wiliam Zacarias da Silva Rosa - wiliamzsr@motta.com.br                       }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{*******************************************************************************
|* Historico
|*
|* 19/08/2009: Wiliam Rosa
|*  - Definição de classes para impressão do DACTE
*******************************************************************************}

{$I ACBr.inc}

unit ACBrCTeDACTEClass;

interface

uses
  SysUtils, Classes, ACBrBase,
  pcteCTE, pcnConversao;

type
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrCTeDACTEClass = class(TACBrComponent)
  private
    function GetPathPDF: String;
    procedure SetPathPDF(const Value: String);
    procedure SetCTE(const Value: TComponent);
    procedure ErroAbstract(NomeProcedure: String);
  protected
    FACBrCTE: TComponent;
    FLogo: String;
    FSistema: String;
    FUsuario: String;
    FPathPDF: String;
    FImpressora: String;
    FImprimirHoraSaida: Boolean;
    FImprimirHoraSaida_Hora: String;
    FMostrarPreview: Boolean;
    FMostrarStatus: Boolean;
    FTipoDACTE: TpcnTipoImpressao;
    FTamanhoPapel: TpcnTamanhoPapel;
    FNumCopias: Integer;
    FExpandirLogoMarca: Boolean;
    FFax: String;
    FSite: String;
    FEmail: String;
	  FProtocoloCTE: String;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FCTeCancelada: Boolean;
    FResumoCanhoto: Boolean;
    FEPECEnviado: Boolean;
    FPosCanhoto: TPosRecibo;
    FImprimirDescPorc: Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDACTE(CTE: TCTE = nil); virtual;
    procedure ImprimirDACTEPDF(CTE: TCTE = nil); virtual;
    procedure ImprimirEVENTO(CTE: TCTe = nil); virtual;
    procedure ImprimirEVENTOPDF(CTE: TCTe = nil); virtual;
    procedure ImprimirINUTILIZACAO(CTE: TCTe = nil); virtual;
    procedure ImprimirINUTILIZACAOPDF(CTE: TCTe = nil); virtual;
  published
    property ACBrCTE: TComponent            read FACBrCTE                write SetCTE;
    property Logo: String                   read FLogo                   write FLogo;
    property Sistema: String                read FSistema                write FSistema;
    property Usuario: String                read FUsuario                write FUsuario;
    property PathPDF: String                read GetPathPDF              write SetPathPDF;
    property Impressora: String             read FImpressora             write FImpressora;
    property ImprimirHoraSaida: Boolean     read FImprimirHoraSaida      write FImprimirHoraSaida;
    property ImprimirHoraSaida_Hora: String read FImprimirHoraSaida_Hora write FImprimirHoraSaida_Hora;
    property MostrarPreview: Boolean        read FMostrarPreview         write FMostrarPreview;
    property MostrarStatus: Boolean         read FMostrarStatus          write FMostrarStatus;
    property TipoDACTE: TpcnTipoImpressao   read FTipoDACTE              write FTipoDACTE;
    property TamanhoPapel: TpcnTamanhoPapel read FTamanhoPapel           write FTamanhoPapel;
    property NumCopias: Integer             read FNumCopias              write FNumCopias;
    property Fax: String                    read FFax                    write FFax;
    property Site: String                   read FSite                   write FSite;
    property Email: String                  read FEmail                  write FEmail;
    property ProtocoloCTE: String           read FProtocoloCTE           write FProtocoloCTE;
    property MargemInferior: Double         read FMargemInferior         write FMargemInferior;
    property MargemSuperior: Double         read FMargemSuperior         write FMargemSuperior;
    property MargemEsquerda: Double         read FMargemEsquerda         write FMargemEsquerda;
    property MargemDireita: Double          read FMargemDireita          write FMargemDireita;
    property ExpandirLogoMarca: Boolean     read FExpandirLogoMarca      write FExpandirLogoMarca default false;
    property CTeCancelada: Boolean          read FCTeCancelada           write FCTeCancelada;
    property ExibirResumoCanhoto: Boolean   read FResumoCanhoto          write FResumoCanhoto;
    property EPECEnviado: Boolean           read FEPECEnviado            write FEPECEnviado;
    property PosCanhoto: TPosRecibo         read FPosCanhoto             write FPosCanhoto default prCabecalho;
    property ImprimirDescPorc: Boolean      read FImprimirDescPorc       write FImprimirDescPorc;
  end;

implementation

uses
  ACBrCTe, ACBrUtil;

constructor TACBrCTeDACTEClass.Create(AOwner: TComponent);
begin
  inherited create(AOwner);

  FACBrCTE    := nil;
  FLogo       := '';
  FSistema    := '';
  FUsuario    := '';
  FPathPDF    := '';
  FImpressora := '';

  FImprimirHoraSaida      := False;
  FImprimirHoraSaida_Hora := '';

  FMostrarPreview := True;
  FMostrarStatus  := True;
  FNumCopias      := 1;

  FFax   := '';
  FSite  := '';
  FEmail := '';

  FProtocoloCTE    := '';

  FMargemInferior := 0.8;
  FMargemSuperior := 0.8;
  FMargemEsquerda := 0.6;
  FMargemDireita  := 0.51;
  FCTeCancelada   := False;

  FResumoCanhoto := False; 
  FEPECEnviado   := False;
end;

destructor TACBrCTeDACTEClass.Destroy;
begin

  inherited Destroy;
end;

procedure TACBrCTeDACTEClass.ImprimirDACTE(CTE: TCTE = nil);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrCTeDACTEClass.ImprimirDACTEPDF(CTE: TCTE = nil);
begin
  ErroAbstract('ImprimirPDF');
end;

procedure TACBrCTeDACTEClass.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrCTE <> nil) and (AComponent is TACBrCTE) then
     FACBrCTE := nil;
end;

procedure TACBrCTeDACTEClass.SetCTE(const Value: TComponent);
var
 OldValue: TACBrCTE;
begin
  if Value <> FACBrCTE then
  begin
     if Value <> nil then
        if not (Value is TACBrCTE) then
           raise Exception.Create('DACTE deve ser do tipo TACBrCTE');

     if Assigned(FACBrCTE) then
        FACBrCTE.RemoveFreeNotification(Self);

     OldValue := TACBrCTE(FACBrCTE);   // Usa outra variavel para evitar Loop Infinito
     FACBrCTE := Value;                // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.DACTE) then
           OldValue.DACTE := nil;

     if Value <> nil then
     begin
        Value.FreeNotification(self);
        TACBrCTE(Value).DACTE := self;
     end;
  end;
end;

procedure TACBrCTeDACTEClass.ErroAbstract(NomeProcedure: String);
begin
  raise Exception.Create(NomeProcedure);
end;

procedure TACBrCTeDACTEClass.ImprimirEVENTO(CTE: TCTe);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrCTeDACTEClass.ImprimirEVENTOPDF(CTE: TCTe);
begin
  ErroAbstract('ImprimirPDF');
end;

procedure TACBrCTeDACTEClass.ImprimirINUTILIZACAO(CTE: TCTe);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrCTeDACTEClass.ImprimirINUTILIZACAOPDF(CTE: TCTe);
begin
  ErroAbstract('ImprimirPDF');
end;

function TACBrCTeDACTEClass.GetPathPDF: String;
var
  dhEmissao: TDateTime;
  DescricaoModelo: String;
  ACTe: TCTe;
begin
  if (csDesigning in ComponentState) then
  begin
    Result := FPathPDF;
    Exit;
  end;

  Result := Trim(FPathPDF);

  if EstaVazio(Result) then  // Se não informou o Diretório para o PDF
  begin
    if Assigned(ACBrCTe) then  // Se tem o componente ACBrCTe
    begin
      if TACBrCTe(ACBrCTe).Conhecimentos.Count > 0 then  // Se tem algum Conhecimento carregado
      begin
        ACTe := TACBrCTe(ACBrCTe).Conhecimentos.Items[0].CTe;
        if TACBrCTe(ACBrCTe).Configuracoes.Arquivos.EmissaoPathCTe then
          dhEmissao := ACTe.Ide.dhEmi
        else
          dhEmissao := Now;

        DescricaoModelo := '';
        if TACBrCTe(ACBrCTe).Configuracoes.Arquivos.AdicionarLiteral then
        begin
           case ACTe.Ide.modelo of
             0: DescricaoModelo := TACBrCTe(FACBrCTe).GetNomeModeloDFe;
             57: DescricaoModelo := 'CTe';
             67: DescricaoModelo := 'CTeOS';
           end;
        end;

        Result := TACBrCTe(FACBrCTe).Configuracoes.Arquivos.GetPath(
                         Result,
                         DescricaoModelo,
                         ACTe.Emit.CNPJ,
                         dhEmissao,
                         DescricaoModelo);
      end;
    end;
  end;

  if EstaVazio(Result) then  // Se não pode definir o Parth, use o Path da Aplicaçao
    Result := ExtractFilePath(ParamStr(0));

  Result := PathWithDelim( Result );

  (*
  Result := PathWithDelim(FPathPDF);

  // Criar diretório conforme configurado para CT-e
  if Assigned(ACBrCTe) then
  begin
    if TACBrCTe(ACBrCTe).Conhecimentos.Count > 0 then
    begin
      ACTe := TACBrCTe(ACBrCTe).Conhecimentos.Items[0].CTe;
      if TACBrCTe(ACBrCTe).Configuracoes.Arquivos.EmissaoPathCTe then
        dhEmissao := ACTe.Ide.dhEmi
      else
        dhEmissao := Now;

      case ACTe.Ide.modelo of
        0: DescricaoModelo := TACBrCTe(FACBrCTe).GetNomeModeloDFe;
        57: DescricaoModelo := 'CTe';
        67: DescricaoModelo := 'CTeOS';
      end;

      Result := PathWithDelim(TACBrCTe(FACBrCTe).Configuracoes.Arquivos.GetPath(
                              Result
                             ,DescricaoModelo
                             ,ACTe.Emit.CNPJ
                             ,dhEmissao
                             ,DescricaoModelo
                             ));
    end;
  end;
  *)
end;

procedure TACBrCTeDACTEClass.SetPathPDF(const Value: String);
begin
  FPathPDF := PathWithDelim(Value);
end;

end.
