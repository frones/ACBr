{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ISSBarueri.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils, MaskUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXInterface, ACBrNFSeXWebserviceBase,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXConversao, ACBrNFSeXConsts;

type
  { Provedor com layout próprio }

  { TNFSeW_ISSBarueri }

  TNFSeW_ISSBarueri = class(TNFSeWClass)
  private
    function LocalPrestacaoServico(const ATipo: TTipoPessoa): String;
  protected
    procedure GerarRegistroTipo1(const AIdentificacaoRemessa: String);
    procedure GerarRegistroTipo2;
    procedure GerarRegistroTipo3;
    procedure GerarRegistroTipo9;
  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
  synacode, synautil,
  ACBrUtil.Strings,
  ACBrConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS da prefeitura de:
//     ISSBarueri
//==============================================================================

{ TNFSeW_ISSBarueri }

procedure TNFSeW_ISSBarueri.GerarRegistroTipo1(const AIdentificacaoRemessa: String);
begin
  FConteudoTxt.Add(
    '1'+ // Tipo do Registro S Numérico 1 1 1 1
    PadRight(NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 7, ' ')+ // Inscrição do Contribuinte S Texto 7 2 8 Inscrição do Prestador de Serviço
    'PMB002'+ // Versão do Lay-Out S Texto 6 9 14 Versão do Lay-Out "PMB002"
    PadLeft(AIdentificacaoRemessa, 11, '0') // Identificação da Remessa do Contribuinte
  );
end;

procedure TNFSeW_ISSBarueri.GerarRegistroTipo2;
var
  Quantidade: Integer;
  SituacaoRPS, CodCancelamento, MotCancelamento: String;
  ValorTotalRetencoes: Double;
begin
  SituacaoRPS := 'E';

  CodCancelamento := '';
  MotCancelamento := '';
  Quantidade := 1;

  if NFSe.StatusRps = srCancelado then
  begin
    SituacaoRPS := 'C';
    CodCancelamento := NFSe.CodigoCancelamento;
    MotCancelamento := NFSe.MotivoCancelamento;
  end;

  if (Assigned(NFSe.Servico.ItemServico)) and
     (Pred(NFSe.Servico.ItemServico.Count) > 0) then
    Quantidade := Trunc(NFSe.Servico.ItemServico.Items[0].Quantidade);

  ValorTotalRetencoes := NFSe.Servico.Valores.ValorIr +
                         NFSe.Servico.Valores.ValorPis +
                         NFSe.Servico.Valores.ValorCofins +
                         NFSe.Servico.Valores.ValorCsll;

  FConteudoTxt.Add(
    '2'+ // Tipo do Registro S Numérico 1 1 1 2
    'RPS  '+ // Tipo do RPS S Texto 5 2 6 RPS
    PadRight(NFSe.IdentificacaoRps.Serie, 4, ' ')+ // Série do RPS N Texto 4 7 10 Série do RPS
    PadRight('', 5, ' ')+ // Série da NF-e S* Texto 5 11 15 Série do NF-e. * Obrigatório somente para contribuintes com regime especial.
    '000'+PadLeft(NFSe.IdentificacaoRps.Numero, 7, '0')+ // Número do RPS Número do RPS, iniciar no número 1, com zeros a esquerda, sendo que obrigatoriamente os 3 primeiros dígitos sejam zero
    FormatDateTime('YYYYMMDD', NFSe.DataEmissaoRps)+ // Data do RPS S AAAAMMDD 8 26 33 Data de Emissão do RPS
    FormatDateTime('HHMMSS', Trunc(NFSe.DataEmissaoRps))+ // Hora do RPS S HHMMSS 6 34 39 Hora de Emissão do RPS
    SituacaoRPS+ // Situação do RPS S Texto 1 40 40 E para RPS Enviado / C para RPS Cancelado
    PadRight(CodCancelamento, 2, ' ')+ // Código de Motivo de Cancelamento S* Texto 2 41 42
    PadRight(IfThen(SituacaoRPS = 'C', NFSe.Numero, ''), 7, ' ')+ // Número da NF-e a ser cancelada/substituida S* Numérico 7 43
    PadRight(IfThen(SituacaoRPS = 'C', NFSe.SeriePrestacao, ''), 5, ' ')+ // Série da NF-e a ser cancelada/substituida N Texto 5 50 54
    PadRight(IfThen(SituacaoRPS = 'C', FormatDateTime('YYYYMMDD', NFSe.DataEmissao), ''), 8, ' ')+ // Data de emissão da NF-e a ser cancelada/substituida S* AAAAMMDD 8 55 62
    PadRight(IfThen(SituacaoRPS = 'C', MotCancelamento, ''), 180, ' ')+ // Descricao do Cancelamento S* Texto 180 63 242
    PadRight(NFSe.Servico.CodigoTributacaoMunicipio, 9, ' ')+ // Código do Serviço Prestado S Numérico 9 243 251
    LocalPrestacaoServico(NFSe.Tomador.IdentificacaoTomador.Tipo)+ // Local da Prestação do Serviço S* Texto 1 252 252
    IfThen(NFSe.Servico.PrestadoEmViasPublicas, '1', '2')+ // Serviço Prestado em Vias Publicas S* Texto 1 253 253
    PadRight(NFSe.Tomador.Endereco.Endereco, 75, ' ')+ // Endereço Logradouro do local do Serviço Prestado S* Texto 75 254 328
    PadRight(NFSe.Tomador.Endereco.Numero, 9, ' ')+ // Numero Logradouro do local do Serviço Prestado S* Texto 9 329 337
    PadRight(NFSe.Tomador.Endereco.Complemento, 30, ' ')+ // Complemento Logradouro do local do Serviço Prestado S* Texto 30 338 367
    PadRight(NFSe.Tomador.Endereco.Bairro, 40, ' ')+ // Bairro Logradouro do local do Serviço Prestado S* Texto 40 368 407
    PadRight(NFSe.Tomador.Endereco.xMunicipio, 40, ' ')+ // Cidade Logradouro do local do Serviço Prestado S* Texto 40 408 447
    PadRight(NFSe.Tomador.Endereco.UF, 2, ' ')+ // UF Logradouro do local do Serviço Prestado S* Texto 2 448 449
    PadRight(NFSe.Tomador.Endereco.CEP, 8, ' ')+ // CEP Logradouro do local do Serviço Prestado S* Texto 8 450 457
    PadLeft(IntToStr(Quantidade), 6, '0')+ // Quantidade de Serviço S Numérico 6 458 463
    PadLeft(FloatToStr(NFSe.Servico.Valores.ValorServicos * 100), 15, '0')+ // Valor do Serviço S Numérico 15 464 478 Exemplo: R$10,25 = 000000000001025
    '     '+ //  Reservado N Texto 5 479 483
    PadLeft(FloatToStr(ValorTotalRetencoes * 100), 15, '0')+ // Valor Total das Retenções S Numérico 15 484 498
    IfThen(Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj) >= 11, '2', '1')+ // Tomador Estrangeiro S Numérico 1 499 499 1 Para Tomador Estrangeiro 2 para Tomador Brasileiro
    PadRight('', 3, ' ')+ // Pais da Nacionalidade do Tomador Estrangeiro S* Numérico 3 500 502 Códido do pais de nacionalidade do tomador, conforme tabela de paises, quando o tomador for estrangeiro
    '2'+ // Serviço Prestado é exportação S* Numérico 1 503 503 1 Para Serviço exportado 2 para Serviço não exportado
    IfThen(Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj) > 11, '2', '1')+ // Indicador do CPF/CNPJ do Tomador, pegar do Pessoas a constante S* Numérico 1 504 504 1 para CPF / 2 para CNPJ
    PadLeft(NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 14, ' ')+ // CPF/ CNPJ do Tomador S* Numérico 14 505 518
    PadRight(NFSe.Tomador.RazaoSocial, 60, ' ')+ // Razão Social / Nome do Tomador S Texto 60 519 578
    PadRight(NFSe.Tomador.Endereco.Endereco, 75, ' ')+ // Endereço Logradouro Tomador S* Texto 75 579 653
    PadRight(NFSe.Tomador.Endereco.Numero, 9, ' ')+ // Numero Logradouro Tomador S* Texto 9 654 662
    PadRight(NFSe.Tomador.Endereco.Complemento, 30, ' ')+ // Complemento Logradouro Tomador S* Texto 30 663 692
    PadRight(NFSe.Tomador.Endereco.Bairro, 40, ' ')+ // Bairro Logradouro Tomador S* Texto 40 693 732
    PadRight(NFSe.Tomador.Endereco.xMunicipio, 40, ' ')+ // Cidade Logradouro Tomador S* Texto 40 733 772
    PadRight(NFSe.Tomador.Endereco.UF, 2, ' ')+ // UF Logradouro Tomador S* Texto 2 773 774
    PadRight(NFSe.Tomador.Endereco.CEP, 8, ' ')+ // CEP Logradouro Tomador S* Texto 8 775 782
    PadRight(NFSe.Tomador.Contato.Email, 152, ' ')+ // e-mail Tomador S* Texto 152 783 934
    PadRight('', 6, ' ')+ // Fatura N Numérico 6 935 940 Número da Fatura
    PadLeft('', 15, ' ')+ // Valor Fatura S* Numérico 15 941 955
    PadRight('', 15, ' ')+ // Forma de Pagamento S* Texto 15 956 970
    PadRight(NFSe.Servico.Discriminacao, 1000, ' ') // Discriminação do Serviço S Texto 1000 971 1970
  );
end;

procedure TNFSeW_ISSBarueri.GerarRegistroTipo3;
begin
  if (NFSe.Servico.Valores.RetidoIr = snSim) and
     (NFSe.Servico.Valores.ValorIr > 0) then
  begin
    FConteudoTxt.Add(
      '3'+ // Tipo do Registro S* Numérico 1 1 1
      '01'+ // Código de Outros Valores S Texto 2 2 3 01 - para IRRF
      PadLeft(FloatToStr(NFSe.Servico.Valores.ValorIr * 100), 15, '0')
    );
  end;

  if (NFSe.Servico.Valores.RetidoPis = snSim) and
     (NFSe.Servico.Valores.ValorPis > 0) then
  begin
    FConteudoTxt.Add(
      '3'+ // Tipo do Registro S* Numérico 1 1 1
      '02'+ // Código de Outros Valores S Texto 2 2 3 02 - para PIS/PASEP
      PadLeft(FloatToStr(NFSe.Servico.Valores.ValorPis * 100), 15, '0')
    );
  end;

  if (NFSe.Servico.Valores.RetidoCofins = snSim) and
     (NFSe.Servico.Valores.ValorCofins > 0) then
  begin
    FConteudoTxt.Add(
      '3'+ // Tipo do Registro S* Numérico 1 1 1
      '03'+ // Código de Outros Valores S Texto 2 2 3 03 - para COFINS
      PadLeft(FloatToStr(NFSe.Servico.Valores.ValorCofins * 100), 15, '0')
    );
  end;

  if (NFSe.Servico.Valores.RetidoCsll = snSim) and
     (NFSe.Servico.Valores.ValorCsll > 0) then
  begin
    FConteudoTxt.Add(
      '3'+ // Tipo do Registro S* Numérico 1 1 1
      '04'+ // Código de Outros Valores S Texto 2 2 3 04 - para CSLL
      PadLeft(FloatToStr(NFSe.Servico.Valores.ValorCsll * 100), 15, '0')
    );
  end;
end;

procedure TNFSeW_ISSBarueri.GerarRegistroTipo9;
var
  ValorTotalRetencoes: Double;
begin
  ValorTotalRetencoes := NFSe.Servico.Valores.ValorIr +
                         NFSe.Servico.Valores.ValorPis +
                         NFSe.Servico.Valores.ValorCofins +
                         NFSe.Servico.Valores.ValorCsll;

  FConteudoTxt.Add(
    '9'+ // Tipo do Registro S Numérico 1 1 1 9
    PadRight(IntToStr(FConteudoTxt.Count + 1), 7, ' ')+ // Número Total de Linhas do Arquivo S Numérico 7 2 8
    PadLeft(FloatToStr(NFSe.Servico.Valores.ValorServicos * 100), 15, '0')+ // Valor Total dos Serviços contidos no Arquivo S Numérico 15 9 23
    PadLeft(FloatToStr(ValorTotalRetencoes * 100), 15, '0') // Valor Total dos Valores contidos no registro 3 S Numérico 15 24 38 Valor Total das Retenções e outros valores informados no registro 3
  );
end;

function TNFSeW_ISSBarueri.LocalPrestacaoServico(const ATipo: TTipoPessoa): String;
begin
  if (ATipo in [tpPFNaoIdentificada, tpPF, tpPJdoMunicipio]) then
    Result := '1'
  else
    Result := '2';
end;

function TNFSeW_ISSBarueri.GerarXml: Boolean;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  FConteudoTxt.Clear;

  {$IFDEF FPC}
  FConteudoTxt.LineBreak := CRLF;
  {$ELSE}
    {$IFDEF DELPHI2006_UP}
    FConteudoTxt.LineBreak := CRLF;
    {$ENDIF}
  {$ENDIF}

  if NFSe.StatusRps = srCancelado then
    GerarRegistroTipo1(FormatDateTime('yyyymmddzzz', Now))
  else
    GerarRegistroTipo1(NFSe.IdentificacaoRps.Numero);

  GerarRegistroTipo2;
  GerarRegistroTipo3;
  GerarRegistroTipo9;

  Result := True;
end;

end.
