{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrPagFor;

interface

uses
  Classes, SysUtils, Contnrs,
  {$IFDEF CLX} QDialogs,{$ELSE} Dialogs,{$ENDIF}
  ACBrBase, ACBrPagForClass, ACBrPagForGravarTxt, ACBrPagForLerTxt, ACBrPagForConversao,
  ACBrPagForArquivo, ACBrPagForArquivoClass, ACBrPagForConfiguracoes;

type
  EACBrPagForException = class(Exception);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPagFor = class(TComponent)
  private
    FArquivo  : TACBrPagForArquivoClass;
    FArquivos : TArquivos;
    FConfiguracoes: TConfiguracoes;

    procedure SetArquivo(const Value: TACBrPagForArquivoClass);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Limpar;
    function GravarTXT(ANomeArquivo: String = ''): Boolean;
    function LerTXT(AArquivoTXT: String; ACarregarArquivo: Boolean): Boolean;

    property Arquivo: TACBrPagForArquivoClass read FArquivo write SetArquivo;
    property Arquivos: TArquivos read FArquivos write FArquivos;
  published
    property Configuracoes: TConfiguracoes  read FConfiguracoes  write FConfiguracoes;
  end;

implementation

{ TACBrPagFor }

constructor TACBrPagFor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FConfiguracoes      := TConfiguracoes.Create( self );
  FConfiguracoes.Name := 'Configuracoes';

  {$IFDEF COMPILER6_UP}
   FConfiguracoes.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}

  FArquivos := TArquivos.Create(Self, TRegistro);
  FArquivos.Configuracoes := FConfiguracoes;
end;

destructor TACBrPagFor.Destroy;
begin
  FConfiguracoes.Free;
  FArquivos.Free;
  inherited;
end;

function TACBrPagFor.GravarTXT(ANomeArquivo: String = ''): Boolean;
begin
  if Self.Arquivos.Count <=0 then
    raise EACBrPagForException.Create('ERRO: Nenhum arquivo adicionado.');

  Result := Self.Arquivos.GerarPagFor(ANomeArquivo);
end;

function TACBrPagFor.LerTXT(AArquivoTXT: String; ACarregarArquivo: Boolean): Boolean;
begin
  FArquivos.Ler(AArquivoTXT, ACarregarArquivo);
  
  Result := True;
end;

procedure TACBrPagFor.Limpar;
begin
  FArquivos.Clear;
  FArquivos.Configuracoes.Limpar;
  FConfiguracoes.Limpar;
end;

procedure TACBrPagFor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FArquivo <> nil) and (AComponent is TACBrPagForArquivoClass) then
     FArquivo := nil;
end;

procedure TACBrPagFor.SetArquivo(const Value: TACBrPagForArquivoClass);
var
 OldValue: TACBrPagForArquivoClass;
begin
  if Value <> FArquivo then
  begin
    if Assigned(FArquivo) then
      FArquivo.RemoveFreeNotification(Self);

    OldValue := FArquivo;   // Usa outra variavel para evitar Loop Infinito
    FArquivo := Value;    // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.ACBrPagFor) then
        OldValue.ACBrPagFor := nil;

    if Value <> nil then
    begin
      Value.FreeNotification(self);
      Value.ACBrPagFor := self;
    end;
  end;
end;

(*
Const

 // Tipo de Serviço
 tsCobranca                       = 01;
 tsBoletoEletronico               = 03;
 tsConciliacaoBancaria            = 04;
 tsDebitos                        = 05;
 tsCustodiaCheques                = 06;
 tsGestaoCaixa                    = 07;
 tsConsultaMargem                 = 08;
 tsAverbacaoConsignacao           = 09;
 tsPagamentoDividendos            = 10;
 tsManutencaoConsignacao          = 11;
 tsConsignacaoParcelas            = 12;
 tsGlosaConsignacaoINSS           = 13;
 tsConsultaTributosaPagar         = 14;
 tsPagamentoFornecedor            = 20;
 tsPagamentoContaTributosImpostos = 22;
 tsCompror                        = 25;
 tsComprorRotativo                = 26;
 tsAlegacaoSacado                 = 29;
 tsPagamentoSalarios              = 30;
 tsPagamentoHonorarios            = 32;
 tsPagamentoBolsaAuxilio          = 33;
 tsPagamentoPrebenda              = 34; // Remuneração a Padres e Sacerdotes
 tsVendor                         = 40;
 tsVendoraTermo                   = 41;
 tsPagamentoSinistrosSegurados    = 50;
 tsPagamentoDespesasViajante      = 60;
 tsPagamentoAutorizado            = 70;
 tsPagamentoCredenciados          = 75;
 tsPagamentoRemuneracao           = 77;
 tsPagamentoRepresentantes        = 80;
 tsPagamentoBeneficios            = 90;
 tsPagamentosDiversos             = 98;

 // Forma de Lancamento
 flCreditoContaCorrente           = 01;
 flChequePagamentoAdministrativo  = 02;
 flDOCTED                         = 03;
 flCartaoSalario                  = 04; // requer tipo de serviço = 30
 flCreditoContaPoupanca           = 05;
 flOPaDisposicao                  = 10;
 flPagamentoContasTributoscomBar  = 11; // com Código de Barras
 flTributoDARFnormal              = 16;
 flTributoGPS                     = 17; // Guia da Previdência Social
 flTributoDARFsimples             = 18;
 flTributoIPTUprefeituras         = 19;
 flPagamentocomAutenticacao       = 20;
 flTributoDARJ                    = 21;
 flTributoGARESPicms              = 22;
 flTributoGARESPdr                = 23;
 flTributoGARESPitcmd             = 24;
 flTributoIPVA                    = 25;
 flTributoLicenciamento           = 26;
 flTributoDPVAT                   = 27;
 flPagamentoTitulosdoBanco        = 30;
 flPagamentoTitulosdeTerceiros    = 31;
 flExtratoContaCorrente           = 40;
 flTEDoutraTitularidade           = 41;
 flTEDmesmaTitularidade           = 43;
 flTEDtransferenciaContaInvest    = 44;
 flDebitoContaCorrente            = 50;
 flExtratoGestaoCaixa             = 70;
 flDepositoJudicialContaCorrente  = 71;
 flDepositoJudicialPoupanca       = 72;
 flExtratoContaInvestimento       = 73;
*)
end.
