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

unit PagFor.Bradesco.GravarTxtRemessa;

interface

uses
  SysUtils, Classes,
  ACBrPagForClass, CNAB240.GravarTxtRemessa;

type
 { TArquivoW_Bradesco }

  TArquivoW_Bradesco = class(TArquivoW_CNAB240)
  protected
    procedure GeraRegistro0; override;

    procedure GeraRegistro1(I: Integer); override;

    procedure GeraSegmentoB(mSegmentoBList: TSegmentoBList); override;

    procedure GeraSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List); override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrPagForConversao;

{ TArquivoW_Bradesco }

procedure TArquivoW_Bradesco.GeraRegistro0;
var LIdentificacaoRemessa : String;
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
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.DV, 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.Nome, 30, tcStr, True);
  GravarCampo(PagFor.Registro0.NomeBanco, 30, tcStr, True);
  GravarCampo(' ', 10, tcStr);
  GravarCampo(TpArquivoToStr(PagFor.Registro0.Arquivo.Codigo), 1, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.DataGeracao, 8, tcDat);
  GravarCampo(PagFor.Registro0.Arquivo.HoraGeracao, 6, tcHor);
  GravarCampo(PagFor.Registro0.Arquivo.Sequencia, 6, tcInt);
  GravarCampo('089', 3, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.Densidade, 5, tcInt);

  if (PagFor.Lote.Count > 0) and (PagFor.Lote[0].Registro1.Servico.FormaLancamento in [flPIXTransferencia, flPIXQRCode]) then
    LIdentificacaoRemessa := 'Pix';

  GravarCampo(LIdentificacaoRemessa, 3, tcStr);
  GravarCampo(PagFor.Registro0.ReservadoBanco, 17, tcStr);
  GravarCampo(PagFor.Registro0.ReservadoEmpresa, 20, tcStr, True);
  GravarCampo(' ', 29, tcStr);

  ValidarLinha('0');
  IncluirLinha;
end;

procedure TArquivoW_Bradesco.GeraRegistro1(I: Integer);
begin
  FpLinha := '';
  Inc(FQtdeRegistros);
  Inc(FQtdeLotes);

  if PagFor.Lote.Items[I].Registro1.Servico.Operacao = toExtrato then
    Inc(FQtdeContasConc);

  FQtdeRegistrosLote := 1;
  FSequencialDeLote  := 0;

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(FQtdeLotes, 4, tcInt);
  GravarCampo(1, 1, tcInt);
  GravarCampo(TpOperacaoToStr(PagFor.Lote.Items[I].Registro1.Servico.Operacao), 1, tcStr);
  GravarCampo(TpServicoToStr(PagFor.Lote.Items[I].Registro1.Servico.TipoServico), 2, tcStr);
  GravarCampo(FmLancamentoToStr(PagFor.Lote.Items[I].Registro1.Servico.FormaLancamento), 2, tcStr);

  if PagFor.Lote.Items[I].SegmentoA.Count > 0 then
    // Se for parte do Header (Pagamentos através de cheque, OP, DOC, TED e crédito em conta corrente)
    // Segmento A - Pagamentos através de cheque, OP, DOC, TED e crédito em conta corrente
    GravarCampo('045', 3, tcStr)
  else
    if PagFor.Lote.Items[I].SegmentoO.Count > 0 then
      GravarCampo('012', 3, tcStr)
    else
      GravarCampo('040', 3, tcStr);

  GravarCampo(' ', 1, tcStr);
  GravarCampo(TpInscricaoToStr(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Tipo), 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Numero, 14, tcStrZero);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Convenio, 20, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.DV, 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.Numero, 12, tcInt);
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

  if (PagFor.Lote.Items[I].SegmentoA.Count > 0) or (PagFor.Lote.Items[I].SegmentoO.Count > 0) then
  begin
    // Se for parte do Header (Pagamentos através de cheque, OP, DOC, TED e crédito em conta corrente)
    // Segmento A - Pagamentos através de cheque, OP, DOC, TED e crédito em conta corrente
    GravarCampo('01', 2, tcStr);
    GravarCampo(' ', 16, tcStr);
  end
  else
    GravarCampo(' ', 18, tcStr);

  ValidarLinha('1');
  IncluirLinha;
end;

procedure TArquivoW_Bradesco.GeraSegmentoB(mSegmentoBList: TSegmentoBList);
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
      Inc(FSequencialDeLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDeLote, 5, tcInt);
      GravarCampo('B', 1, tcStr);

      if PixTipoChave <> tcpNenhum then
      begin
        GravarCampo(TipoChavePixToStr(PixTipoChave), 2, tcStr);
        GravarCampo(' ', 1, tcStr);
        GravarCampo(TpInscricaoToStr(Inscricao.Tipo), 1, tcStr);
        GravarCampo(Inscricao.Numero, 14, tcStrZero);
        GravarCampo(PixTXID, 35, tcStr);
        GravarCampo(PixMensagem, 60, tcStr);
        GravarCampo(PixChave, 99, tcStr);
        GravarCampo(CodigoUG, 6, tcInt);
        GravarCampo(CodigoISPB, 8, tcInt);
      end
      else
      begin
        GravarCampo(' ', 3, tcStr);
        GravarCampo(TpInscricaoToStr(Inscricao.Tipo), 1, tcStr);
        GravarCampo(Inscricao.Numero, 14, tcStrZero);
        GravarCampo(Endereco.Logradouro, 30, tcStr, True);
        GravarCampo(Endereco.Numero, 5, tcStrZero);
        GravarCampo(Endereco.Complemento, 15, tcStr, True);
        GravarCampo(Endereco.Bairro, 15, tcStr, True);
        GravarCampo(Endereco.Cidade, 20, tcStr, True);
        GravarCampo(Endereco.CEP, 8, tcInt);
        GravarCampo(Endereco.Estado, 2, tcStr);
        GravarCampo(DataVencimento, 8, tcDat);
        GravarCampo(Valor, 15, tcDe2);
        GravarCampo(Abatimento, 15, tcDe2);
        GravarCampo(Desconto, 15, tcDe2);
        GravarCampo(Mora, 15, tcDe2);
        GravarCampo(Multa, 15, tcDe2);
        GravarCampo(CodigoDOC, 15, tcStr);
        GravarCampo(Aviso, 1, tcInt);
        GravarCampo(CodigoUG, 6, tcInt);
        GravarCampo(' ', 8, tcStr);
      end;

      ValidarLinha('B');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_Bradesco.GeraSegmentoJ52(
  mSegmentoJ52List: TSegmentoJ52List);
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
      Inc(FSequencialDeLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDeLote, 5, tcInt);
      GravarCampo('J', 1, tcStr);
      GravarCampo(' ', 1, tcStr);
      GravarCampo('00', 2, tcStr);
      GravarCampo('52', 2, tcStr);
      GravarCampo(TpInscricaoToStr(Pagador.Inscricao.Tipo), 1, tcStr);
      GravarCampo(Pagador.Inscricao.Numero, 15, tcStrZero);
      GravarCampo(Pagador.Nome, 40, tcStr, True);
      GravarCampo(TpInscricaoToStr(Beneficiario.Inscricao.Tipo), 1, tcStr);
      GravarCampo(Beneficiario.Inscricao.Numero, 15, tcStrZero);
      GravarCampo(Beneficiario.Nome, 40, tcStr, True);

      if Chave = '' then
      begin
        GravarCampo(TpInscricaoToStr(SacadorAvalista.Inscricao.Tipo), 1, tcStr);
        GravarCampo(SacadorAvalista.Inscricao.Numero, 15, tcStrZero);
        GravarCampo(SacadorAvalista.Nome, 40, tcStr, True);
        GravarCampo(' ', 53, tcStr);
      end
      else
      begin
        GravarCampo(Chave, 79, tcStr);
        GravarCampo(TXID, 30, tcStr);
      end;

      ValidarLinha('J52');
      IncluirLinha;
    end;
  end;
end;

end.
