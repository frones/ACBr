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

unit PagFor.Caixa.GravarTxtRemessa;

interface

uses
  SysUtils, Classes,
  ACBrPagForClass,
  CNAB240.GravarTxtRemessa;

type
 { TArquivoW_Caixa }

  TArquivoW_Caixa = class(TArquivoW_CNAB240)
  protected
    procedure GeraRegistro0; override;

    procedure GeraRegistro1(I: Integer); override;

    procedure GeraSegmentoA(I: Integer); override;

    procedure GeraSegmentoB(mSegmentoBList: TSegmentoBList); override;

    procedure GeraSegmentoJ(I: Integer); override;

    procedure GeraSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List); override;
  end;

implementation

uses
  ACBrPagForConversao;

{ TArquivoW_Caixa }

procedure TArquivoW_Caixa.GeraRegistro0;
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
  GravarCampo(PagFor.Registro0.Empresa.Convenio, 6, tcStrZero);
  GravarCampo(PagFor.Registro0.Arquivo.ParamTransm, 2, tcStrZero);
  GravarCampo(PagFor.Geral.AmbienteCliente, 1, tcStr);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(' ', 3, tcStr);
  GravarCampo(0, 4, tcInt);
  GravarCampo(' ', 3, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Agencia.DV, 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Conta.Numero, 12, tcInt64);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Conta.DV, 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.DV, 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.Nome, 30, tcStr, True);
  GravarCampo(PagFor.Registro0.NomeBanco, 30, tcStr, True);
  GravarCampo(' ', 10, tcStr);
  GravarCampo(TpArquivoToStr(PagFor.Registro0.Arquivo.Codigo), 1, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.DataGeracao, 8, tcDat);
  GravarCampo(PagFor.Registro0.Arquivo.HoraGeracao, 6, tcHor);
  GravarCampo(PagFor.Registro0.Arquivo.Sequencia, 6, tcInt);
  GravarCampo('080', 3, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.Densidade, 5, tcInt);
  GravarCampo(PagFor.Registro0.ReservadoBanco, 20, tcStr);
  GravarCampo(PagFor.Registro0.ReservadoEmpresa, 20, tcStr, True);
  GravarCampo(' ', 14, tcStr);
  GravarCampo(0, 3, tcInt);
  GravarCampo(' ', 12, tcStr);

  ValidarLinha('0');
  IncluirLinha;
end;

procedure TArquivoW_Caixa.GeraRegistro1(I: Integer);
var
  Versao: string;
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
  {
  case FpFormaLancamento of
    flCreditoContaCorrente, flChequePagamento, flDocTed, flOPDisposicao,
    flPagamentoAutenticacao:
      Versao := '046';

    flLiquidacaoTitulosOutrosBancos:
      Versao := '040';

    flTributoDARFNormal, flTributoGPS, flTributoDARFSimples, flTributoIPTU,
    flTributoDARJ, flTributoGARESPICMS, flTributoGARESPDR, flTributoGARESPITCMD,
    flTributoIPVA, flTributoLicenciamento, flTributoDPVAT, flTributoGNRe:
      Versao := '012';
  else
    Versao := '000';
  end;
  }
  Versao := '041';

  GravarCampo(Versao, 3, tcStr);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(TpInscricaoToStr(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Tipo), 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Numero, 14, tcStrZero);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Convenio, 6, tcStrZero);
  GravarCampo(PagFor.Lote.Items[I].Registro1.TipoCompromisso, 2, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.CodigoCompromisso, 4, tcInt);
  GravarCampo(PagFor.Registro0.Arquivo.ParamTransm, 2, tcStrZero);
  GravarCampo(' ', 6, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.DV, 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.Numero, 12, tcInt64);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV, 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.DV, 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Nome, 30, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Informacao1, 40, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Logradouro, 30, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Numero, 5, tcStrZero);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Complemento, 15, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Cidade, 20, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.CEP, 8, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Estado, 2, tcStr);
  {
  case PagFor.Lote.Items[I].Registro1.Servico.FormaLancamento of
    flLiquidacaoTitulosOutrosBancos:
      GravarCampo(' ', 2, tcStr);
  else
    GravarCampo(IndFormaPagToStr(PagFor.Lote.Items[I].Registro1.IndFormaPag), 2, tcStr);
  end;
  }
  GravarCampo(' ', 8, tcStr);
  GravarCampo(' ', 10, tcStr);

  ValidarLinha('1');
  IncluirLinha;
end;

procedure TArquivoW_Caixa.GeraSegmentoA(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoA.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoA.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDoRegistroNoLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
      GravarCampo('A', 1, tcStr);
      GravarCampo(TpMovimentoToStr(TipoMovimento), 1, tcStr);
      GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
      GravarCampo(Favorecido.Camara, 3, tcInt);
      GravarCampo(BancoToStr(Favorecido.Banco), 3, tcStr);
      GravarCampo(Favorecido.ContaCorrente.Agencia.Codigo, 5, tcInt);
      GravarCampo(Favorecido.ContaCorrente.Agencia.DV, 1, tcStr);
      GravarCampo(Favorecido.ContaCorrente.Conta.Numero, 12, tcInt64);
      GravarCampo(Favorecido.ContaCorrente.Conta.DV, 1, tcStr);
      GravarCampo(Favorecido.ContaCorrente.DV, 1, tcStr);
      GravarCampo(Favorecido.Nome, 30, tcStr, True);
      GravarCampo(Credito.SeuNumero, 6, tcStrZero);
      GravarCampo(' ', 13, tcStr);
      GravarCampo(Favorecido.ContaCorrente.Conta.TipoConta, 1, tcInt);
      GravarCampo(Credito.DataPagamento, 8, tcDat);
      GravarCampo(TpMoedaToStr(Credito.Moeda.Tipo), 3, tcStr);
      GravarCampo(Credito.Moeda.Qtde, 15, tcDe5);
      GravarCampo(Credito.ValorPagamento, 15, tcDe2);
      GravarCampo(Credito.NossoNumero, 9, tcStrZero);
      GravarCampo(' ', 3, tcStr);
      GravarCampo(Credito.QtdeParcelas, 2, tcInt);
      GravarCampo(Credito.IndBloqueio, 1, tcStr);
      GravarCampo(Credito.FormaParcelamento, 1, tcInt);
      GravarCampo(Credito.DiaVencimento, 2, tcInt);
      GravarCampo(Credito.NumParcela, 2, tcInt);
      GravarCampo(Credito.DataReal, 8, tcDat);
      GravarCampo(Credito.ValorReal, 15, tcDe2);
      GravarCampo(Informacao2, 40, tcStr, True);
      GravarCampo(CodigoDOC, 2, tcStr);
      GravarCampo(' ', 10, tcStr);
      GravarCampo(Aviso, 1, tcInt);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('A');
      IncluirLinha;

      {opcionais do segmento A}
      GeraSegmentoB(SegmentoB);
      GeraSegmentoC(SegmentoC);
    end;
  end;
end;

procedure TArquivoW_Caixa.GeraSegmentoB(mSegmentoBList: TSegmentoBList);
var
  J: Integer;
begin
  for J := 0 to mSegmentoBList.Count - 1 do
  begin
    FpLinha := '';

    with mSegmentoBList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDoRegistroNoLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
      GravarCampo('B', 1, tcStr);

      if (PagFor.Lote.Items[0].SegmentoN1.Count > 0) or
         (PagFor.Lote.Items[0].SegmentoN2.Count > 0) then
        GravarCampo(' ', 18, tcStr)
      else
      begin
        GravarCampo(' ', 3, tcStr);
        GravarCampo(TpInscricaoToStr(Inscricao.Tipo), 1, tcStr);
        GravarCampo(Inscricao.Numero, 14, tcStrZero);
      end;

      GravarCampo(Endereco.Logradouro, 30, tcStr, True);
      GravarCampo(Endereco.Numero, 5, tcStrZero);
      GravarCampo(Endereco.Complemento, 15, tcStr, True);
      GravarCampo(Endereco.Bairro, 15, tcStr, True);
      GravarCampo(Endereco.Cidade, 20, tcStr, True);
      GravarCampo(Endereco.CEP, 8, tcInt);
      GravarCampo(Endereco.Estado, 2, tcStr);

      if (PagFor.Lote.Items[0].SegmentoN1.Count > 0) or
         (PagFor.Lote.Items[0].SegmentoN2.Count > 0) then
      begin
        GravarCampo(Telefone, 11, tcStr);
        GravarCampo(' ', 111, tcStr);
      end
      else
      begin
        GravarCampo(DataVencimento, 8, tcDat);
        GravarCampo(Valor, 15, tcDe2);
        GravarCampo(Abatimento, 15, tcDe2);
        GravarCampo(Desconto, 15, tcDe2);
        GravarCampo(Mora, 15, tcDe2);
        GravarCampo(Multa, 15, tcDe2);
        GravarCampo(CodigoDOC, 15, tcStr);
        GravarCampo(' ', 15, tcStr);
      end;

      ValidarLinha('B');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_Caixa.GeraSegmentoJ(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoJ.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoJ.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDoRegistroNoLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
      GravarCampo('J', 1, tcStr);
      GravarCampo(TpMovimentoToStr(TipoMovimento), 1, tcStr);
      GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
      GravarCampo(CodigoBarras, 44, tcStr);
      GravarCampo(NomeCedente, 30, tcStr, True);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(ValorTitulo, 15, tcDe2);
      GravarCampo(Desconto, 15, tcDe2);
      GravarCampo(Acrescimo, 15, tcDe2);
      GravarCampo(DataPagamento, 8, tcDat);
      GravarCampo(ValorPagamento, 15, tcDe2);
      GravarCampo(QtdeMoeda, 15, tcDe5);
      GravarCampo(SeuNumero, 6, tcStrZero);
      GravarCampo(' ', 14, tcStr);
      GravarCampo(NossoNumero, 9, tcStr);
      GravarCampo(' ', 11, tcStr);
      GravarCampo(CodigoMoeda, 2, tcInt);
      GravarCampo(' ', 6, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('J');
      IncluirLinha;

      {opcionais segmento J}
      GeraSegmentoJ52(SegmentoJ52);
//      GeraSegmentoB(SegmentoB);
//      GeraSegmentoC(SegmentoC);
    end;
  end;
end;

procedure TArquivoW_Caixa.GeraSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List);
var
  J: Integer;
begin
  for J := 0 to mSegmentoJ52List.Count - 1 do
  begin
    FpLinha := '';

    with mSegmentoJ52List.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDoRegistroNoLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
      GravarCampo('J', 1, tcStr);
      GravarCampo(' ', 1, tcStr);
      GravarCampo(' ', 2, tcStr);
      GravarCampo('52', 2, tcStr);
      GravarCampo(TpInscricaoToStr(Pagador.Inscricao.Tipo), 1, tcStr);
      GravarCampo(Pagador.Inscricao.Numero, 15, tcStrZero);
      GravarCampo(Pagador.Nome, 40, tcStr, True);
      GravarCampo(TpInscricaoToStr(Beneficiario.Inscricao.Tipo), 1, tcStr);
      GravarCampo(Beneficiario.Inscricao.Numero, 15, tcStrZero);
      GravarCampo(Beneficiario.Nome, 40, tcStr, True);

      GravarCampo(TpInscricaoToStr(SacadorAvalista.Inscricao.Tipo), 1, tcStr);
      GravarCampo(SacadorAvalista.Inscricao.Numero, 15, tcStrZero);
      GravarCampo(SacadorAvalista.Nome, 40, tcStr, True);
      GravarCampo(' ', 53, tcStr);

      ValidarLinha('J52');
      IncluirLinha;
    end;
  end;
end;

end.
