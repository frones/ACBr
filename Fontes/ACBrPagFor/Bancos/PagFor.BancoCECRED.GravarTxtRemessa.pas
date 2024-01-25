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

unit PagFor.BancoCECRED.GravarTxtRemessa;

interface

uses
  SysUtils, Classes,
  ACBrPagForClass, CNAB240.GravarTxtRemessa;

type
 { TArquivoW_BancoCECRED }

  TArquivoW_BancoCECRED = class(TArquivoW_CNAB240)
  protected
    procedure GeraRegistro0; override;

    procedure GeraRegistro1(I: Integer); override;

    procedure GeraRegistro5(I: Integer); override;

    procedure GeraRegistro9; override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrPagForConversao;

{ TArquivoW_BancoCECRED }

procedure TArquivoW_BancoCECRED.GeraRegistro0;
begin
  FpLinha := '';
  FQtdeRegistros := 1;
  FQtdeLotes := 0;
  FQtdeContasConc := 0;

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(0, 4, tcInt);
  GravarCampo(0, 1, tcInt);
  GravarCampo(' ', 9, tcStr);
  GravarCampo(TpInscricaoToStr(PagFor.Registro0.Empresa.Inscricao.Tipo), 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.Inscricao.Numero, 14, tcStrZero);
  GravarCampo(PagFor.Registro0.Empresa.Convenio, 20, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Agencia.DV, 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Conta.Numero, 12, tcInt);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Conta.DV, 1, tcStr);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.Nome, 30, tcStr, True);
  GravarCampo(PagFor.Registro0.NomeBanco, 30, tcStr, True);
  GravarCampo(' ', 10, tcStr);
  GravarCampo(TpArquivoToStr(PagFor.Registro0.Arquivo.Codigo), 1, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.DataGeracao, 8, tcDat);
  GravarCampo(PagFor.Registro0.Arquivo.HoraGeracao, 6, tcHor);
  GravarCampo(PagFor.Registro0.Arquivo.Sequencia, 6, tcInt);
  GravarCampo('088', 3, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.Densidade, 5, tcInt);
  GravarCampo(PagFor.Registro0.ReservadoBanco, 20, tcStr);
  GravarCampo(PagFor.Registro0.ReservadoEmpresa, 20, tcStr, True);
  GravarCampo(' ', 29, tcStr);

  ValidarLinha('0');
  IncluirLinha;
end;

procedure TArquivoW_BancoCECRED.GeraRegistro1(I: Integer);
begin
  FpLinha := '';
  Inc(FQtdeRegistros);
  Inc(FQtdeLotes);

  if PagFor.Lote.Items[I].Registro1.Servico.Operacao = toExtrato then
    Inc(FQtdeContasConc);

  FQtdeRegistrosLote := 1;
  FSequencialDoRegistroNoLote := 0;

  FpFormaLancamento := PagFor.Lote.Items[I].Registro1.Servico.FormaLancamento;

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(FQtdeLotes, 4, tcInt);
  GravarCampo(1, 1, tcInt);
  GravarCampo(TpOperacaoToStr(PagFor.Lote.Items[I].Registro1.Servico.Operacao), 1, tcStr);
  GravarCampo(TpServicoToStr(PagFor.Lote.Items[I].Registro1.Servico.TipoServico), 2, tcStr);
  GravarCampo(FmLancamentoToStr(FpFormaLancamento), 2, tcStr);
  GravarCampo('045', 3, tcStr);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(TpInscricaoToStr(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Tipo), 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Numero, 14, tcStrZero);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Convenio, 20, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.DV, 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.Numero, 12, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV, 1, tcStr);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Nome, 30, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Informacao1, 40, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Logradouro, 30, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Numero, 5, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Complemento, 15, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Cidade, 20, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.CEP, 8, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Estado, 2, tcStr);
  GravarCampo(' ', 2, tcStr);
  GravarCampo(' ', 6, tcStr);
  GravarCampo(' ', 10, tcStr);

  ValidarLinha('1');
  IncluirLinha;
end;

procedure TArquivoW_BancoCECRED.GeraRegistro5(I: Integer);
begin
  FpLinha := '';
  Inc(FQtdeRegistros);
  Inc(FQtdeRegistrosLote);

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(FQtdeLotes, 4, tcInt);
  GravarCampo('5', 1, tcStr);
  GravarCampo(' ', 9, tcStr);
  GravarCampo(FQtdeRegistrosLote, 6, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro5.Valor, 18, tcDe2);
  GravarCampo('0', 18, tcStrZero);
  GravarCampo('0', 6, tcStrZero);
  GravarCampo(' ', 165, tcStr);
  GravarCampo(' ', 10, tcStr);

  ValidarLinha('5');
  IncluirLinha;
end;

procedure TArquivoW_BancoCECRED.GeraRegistro9;
begin
  FpLinha := '';
  Inc(FQtdeRegistros);

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(9999, 4, tcInt);
  GravarCampo('9', 1, tcStr);
  GravarCampo(' ', 9, tcStr);
  GravarCampo(FQtdeLotes, 6, tcInt);
  GravarCampo(FQtdeRegistros, 6, tcInt);
  GravarCampo('0', 6, tcStrZero);
  GravarCampo(' ', 205, tcStr);

  ValidarLinha('9');
  IncluirLinha;
end;

end.
