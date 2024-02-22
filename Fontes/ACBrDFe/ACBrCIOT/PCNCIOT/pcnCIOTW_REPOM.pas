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

unit pcnCIOTW_REPOM;

interface

uses
{$IFDEF FPC}
  LResources, 
  Controls,
{$ELSE}

{$ENDIF}
  SysUtils, 
  Classes, 
  StrUtils,
  synacode, 
  ACBrConsts,
  pcnCIOTW, 
  pcnCIOTR,
  pcnGerador, 
  pcnLeitor,
  pcnCIOT,
  ACBrCIOTConversao;

type
  { TCIOTW_REPOM }

  TCIOTW_REPOM = class(TCIOTWClass)
  private
    FVersaoDF: TVersaoCIOT;
  protected
    procedure GerarViagem;
    procedure GerarImpostos;
    procedure GerarPagamentos;
    procedure GerarContratado;
    procedure GerarMotorista;
    procedure GerarDestinatario;
    procedure GerarContratante;
    procedure GerarSubContratante;
    procedure GerarConsignatario;
    procedure GerarTomadorServico;
    procedure GerarVeiculos; 
  public
    constructor Create(ACIOTW: TCIOTW); override;

    property VersaoDF: TVersaoCIOT   read FVersaoDF write FVersaoDF;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;
  end;

const
  DSC_USUARIO = 'login: nome do usuário';
  DSC_SENHA = 'login: senha do usuário';
  DSC_CODATM = 'login: codigo AT&M';
  DSC_APLICACAO = 'Nome da Aplicação';
  DSC_ASSUNTO = 'Assunto do e-mail';
  DSC_REMETENTES = 'Remetentes do e-mail';
  DSC_DESTINATARIOS = 'Destinatários do e-mail';
  DSC_CORPO = 'Corpo do e-mail';
  DSC_CHAVE = 'Chave';
  DSC_CHAVERESP = 'Chave Resposta';
  {
  NAME_SPACE_BASE = 'xmlns="http://schemas.ipc.adm.br/REPOM/pef"';

  NAME_SPACE_REPOM_OBJECTS = 'xmlns="http://schemas.ipc.adm.br/REPOM/objects"';
  NAME_SPACE_REPOM_PEFOBTER_OBJECTS = 'xmlns="http://schemas.ipc.adm.br/REPOM/pef/ObterOperacaoTransporteObjects"';
  NAME_SPACE_REPOM_PEFADICIONAR_OBJECTS = 'xmlns="http://schemas.ipc.adm.br/REPOM/pef/AdicionarOperacaoTransporte"';
  NAME_SPACE_REPOM_PEFADICIONAR_VIAGEM = 'xmlns="http://schemas.ipc.adm.br/REPOM/pef/AdicionarViagem"';
  NAME_SPACE_REPOM_PEFADICIONAR_PAGAMENTOS = 'xmlns="http://schemas.ipc.adm.br/REPOM/pef/AdicionarPagamento"';
  NAME_SPACE_REPOM_PEFENCERRAR_OPERACAO = 'xmlns="http://schemas.ipc.adm.br/REPOM/pef/EncerrarOperacaoTransporte"';

  NAME_SPACE_REPOM_PEFRETIFICAR_OBJECTS = 'xmlns="http://schemas.ipc.adm.br/REPOM/pef/RetificarOperacaoTransporte"';
  NAME_SPACE_REPOM_OPERACAOTRANSPORTE_REPOM = 'xmlns="http://schemas.ipc.adm.br/REPOM/pef/objects"';

  NAME_SPACE_REPOM_VEICULOS_REPOM = 'xmlns="http://schemas.ipc.adm.br/REPOM/veiculos/objects"';
  NAME_SPACE_REPOM_MOTORISTAS_REPOM = 'xmlns="http://schemas.ipc.adm.br/REPOM/motoristas/objects"';
  NAME_SPACE_REPOM_PROPRIETARIOS_REPOM = 'xmlns="http://schemas.ipc.adm.br/REPOM/proprietarios/objects"';
  }
implementation

constructor TCIOTW_REPOM.Create(ACIOTW: TCIOTW);
begin
  inherited Create(ACIOTW);
end;

function TCIOTW_REPOM.ObterNomeArquivo: String;
begin
//  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

procedure TCIOTW_REPOM.GerarSubContratante;
begin
  // Implementar
end;

procedure TCIOTW_REPOM.GerarTomadorServico;
begin
  // Implementar
end;

procedure TCIOTW_REPOM.GerarConsignatario;
begin
  // Implementar
end;

procedure TCIOTW_REPOM.GerarContratado;
begin
  // Implementar
end;

procedure TCIOTW_REPOM.GerarContratante;
begin
  // Implementar
end;

procedure TCIOTW_REPOM.GerarDestinatario;
begin
  // Implementar
end;

procedure TCIOTW_REPOM.GerarImpostos;
begin
  // Implementar
end;

procedure TCIOTW_REPOM.GerarMotorista;
begin
  // Implementar
end;

procedure TCIOTW_REPOM.GerarPagamentos;
begin
  // Implementar
end;

procedure TCIOTW_REPOM.GerarVeiculos;
begin
  // Implementar
end;

procedure TCIOTW_REPOM.GerarViagem;
begin
  // Implementar
end;

function TCIOTW_REPOM.GerarXml: Boolean;
//var
//  Ok: Boolean;
//  versao: Integer;
begin
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';
  // Carrega Layout que sera utilizado para gera o txt
  Gerador.LayoutArquivoTXT.Clear;
  Gerador.ArquivoFormatoTXT := '';

//  VersaoDF := DblToVersaoCIOT(Ok, CIOT.OperacaoTransporte.Versao);
//  versao := VersaoCIOTToInt(VersaoDF);

  // Implementar

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
