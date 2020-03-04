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

unit pcnConversaoCIOT;

interface

uses
  SysUtils, StrUtils, Classes;

type
  TVersaoCIOT = (ve500);

  TCIOTIntegradora = (iNone, ieFrete, iRepom, iPamcard);

  TStatusACBrCIOT = (stCIOTIdle, stCIOTEnviar, stCIOTRetEnviar, stCIOTEmail,
                     stCIOTEnvioWebService);

  TLayOutCIOT = (LayeFreteLogon, layeFreteProprietarios, LayeFreteVeiculos,
                 LayeFreteMotoristas, LayeFreteOperacaoTransporte,
                 LayeFreteFaturamentoTransportadora, LayCIOTRetEnviar);

  TSchemaCIOT = (schErro, schEnviar, schEnviarRetorno);

  TpOperacao = (opLogin, opLogout,
                opGravarProprietario, opGravarVeiculo, opGravarMotorista,
                opAdicionar, opAdicionarViagem, opAdicionarPagamento,
                opObterCodigoIOT , opObterPdf, opRetificar, opCancelar,
                opCancelarPagamento, opEncerrar, opConsultarTipoCarga,
                opAlterarDataLiberacaoPagamento);

  tpTipoConta = (tcContaCorrente, tcContaPoupanca, tcContaPagamentos);

  tpTipoEmbalagem = (teBigbag, tePallet, teGranel, teContainer, teSaco);

  TpTipoViagem = (Indefinido, Padrao, TAC_Agregado, Frota);

  TpUnidadeDeMedidaDaMercadoria = (umIndefinido, umTonelada, umKg);

  TpViagemTipoDeCalculo = (SemQuebra, QuebraSomenteUltrapassado, QuebraIntegral);

  TpTipoProporcao = (tpNenhum, tpPorcentagem, tpValorAbsoluto);

  TpDiferencaFrete = (SemDiferenca, SomenteUltrapassado, Integral);

  TpDiferencaFreteBaseCalculo = (QuantidadeDesembarque, QuantidadeMenor);

  TpTipoPagamento = (TransferenciaBancaria, eFRETE);

  TpTipoCategoriaPagamento = (tcpAdiantamento, tcpEstadia, tcpQuitacao,
                              tcpSemCategoria, tcpFrota);

  TpEntregaDocumentacao = (edRedeCredenciada, edCliente);

  TpTipoRodado = (trNaoAplicavel, trTruck, trToco, trCavalo);

  TpTipoCarroceria = (tcNaoAplicavel, tcAberta, tcFechadaOuBau, tcGranelera,
                      tcPortaContainer, tcSider);

  tpTipoPessoa = (tpIndefinido, tpFisica, tpJuridica);

  TpTipoProprietario = (tpTAC, tpETC, tpCTC);

  //  TAC – Transportador Autônomo de Cargas;
  //  ETC – Empresa de Transporte Rodoviário de Cargas;
  //  CTC – Cooperativa de Transporte Rodoviário de Cargas;

  tpEstadoCIOT = (ecEmViagem, ecEncerrado, ecCancelado);

  tpTipoCarga = (tpGranelsolido, tpGranelLiquido, tpFrigorificada, tpConteinerizada, tpCargaGeral,
                 tpNeogranel, tpPerigosaGranelSolido, tpPerigosaGranelLiquido, tpPerigosaCargaFrigorificada,
                 tpPerigosaConteinerizada, tpPerigosaCargaGeral);

const
  NAME_SPACE_CIOT  = '';

function LayOutToSchema(const t: TLayOutCIOT): TSchemaCIOT;

function LayOutToServico(const t: TLayOutCIOT): String;
function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutCIOT;

function SchemaCIOTToStr(const t: TSchemaCIOT): String;
function StrToSchemaCIOT(out ok: Boolean; const s: String): TSchemaCIOT;

function VersaoCIOTToStr(const t: TVersaoCIOT): String;
function StrToVersaoCIOT(out ok: Boolean; const s: String): TVersaoCIOT;

function VersaoCIOTToInt(const t: TVersaoCIOT): Integer;
function VersaoCIOTToDbl(const t: TVersaoCIOT): Double;
function DblToVersaoCIOT(out ok: Boolean; const d: Double): TVersaoCIOT;

function TipoContaToStr(const t: tpTipoConta): string;

function TipoEmbalagemToStr(const t: tpTipoEmbalagem): string;
function StrToTipoEmbalagem(out ok: Boolean; const s: String): tpTipoEmbalagem;

function TipoViagemCIOTToStr(const t: TpTipoViagem): string;

function TpPagamentoToStr(const t: TpTipoPagamento): string;
function StrToTpPagamento(out ok: Boolean; const s: String): TpTipoPagamento;

function TpUnMedMercToStr(const t: TpUnidadeDeMedidaDaMercadoria): string;

function TpVgTipoCalculoToStr(const t: TpViagemTipoDeCalculo): string;

function TpProporcaoToStr(const t: TpTipoProporcao): string;
function StrToTpProporcao(out ok: Boolean; const s: String): TpTipoProporcao;

function TpDifFreteToStr(const t: TpDiferencaFrete): string;
function StrToTpDifFrete(out ok: Boolean; const s: String): TpDiferencaFrete;

function TpDiferencaFreteBCToStr(const t: TpDiferencaFreteBaseCalculo): string;
function StrToTpDiferencaFreteBC(out ok: Boolean; const s: String): TpDiferencaFreteBaseCalculo;

function TpCatPagToStr(const t: TpTipoCategoriaPagamento): string;

function TpOperacaoToStr(const t: TpOperacao): string;

function EntregaDocumentacaoToStr(const t: TpEntregaDocumentacao): string;

function TipoRodadoToStr(const t: TpTipoRodado): string;
function StrToTipoRodado(out ok: Boolean; const s: String): TpTipoRodado;

function TipoCarroceriaToStr(const t: TpTipoCarroceria): string;
function StrToTipoCarroceria(out ok: Boolean; const s: String): TpTipoCarroceria;

function TipoPessoaToStr(const t: tpTipoPessoa): string;
function StrToTipoPessoa(out ok: Boolean; const s: String): tpTipoPessoa;

function TipoProprietarioToStr(const t: TpTipoProprietario): string;
function StrToTipoProprietario(out ok: Boolean; const s: String): TpTipoProprietario;

function EstadoCIOTToStr(const t: TpEstadoCIOT): string;
function StrToEstadoCIOT(out ok: Boolean; const s: String): TpEstadoCIOT;

function TipoCargaToStr(const t: tpTipoCarga): string;
function StrToTipoCarga(out ok: Boolean; const s: String): tpTipoCarga;

function StrToEnumIntegradora(out ok: Boolean; const s: String): TCIOTIntegradora;

implementation

uses
  typinfo, pcnConversao, pcnAuxiliar;

function LayOutToSchema(const t: TLayOutCIOT): TSchemaCIOT;
begin
  case t of
    LayeFreteLogon,
    layeFreteProprietarios,
    LayeFreteVeiculos,
    LayeFreteMotoristas,
    LayeFreteOperacaoTransporte,
    LayeFreteFaturamentoTransportadora: Result := schEnviar;

    LayCIOTRetEnviar:                   Result := schEnviarRetorno;
  else
    Result := schErro;
  end;
end;

function LayOutToServico(const t: TLayOutCIOT): String;
begin
  Result := EnumeradoToStr(t,
    ['eFreteLogon', 'eFreteProprietarios', 'eFreteVeiculos', 'eFreteMotoristas',
     'eFreteOperacaoTransporte', 'eFreteFaturamentoTransportadora', 'CIOTRetEnviar'],
    [LayeFreteLogon, layeFreteProprietarios, LayeFreteVeiculos, LayeFreteMotoristas,
     LayeFreteOperacaoTransporte, LayeFreteFaturamentoTransportadora, LayCIOTRetEnviar] );
end;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutCIOT;
begin
  Result := StrToEnumerado(ok, s,
  ['eFreteLogon', 'eFreteProprietarios', 'eFreteVeiculos', 'eFreteMotoristas',
   'eFreteOperacaoTransporte', 'eFreteFaturamentoTransportadora', 'CIOTRetEnviar'],
  [LayeFreteLogon, layeFreteProprietarios, LayeFreteVeiculos, LayeFreteMotoristas,
   LayeFreteOperacaoTransporte, LayeFreteFaturamentoTransportadora, LayCIOTRetEnviar] );
end;

function SchemaCIOTToStr(const t: TSchemaCIOT): String;
begin
  Result := GetEnumName(TypeInfo(TSchemaCIOT), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaCIOT(out ok: Boolean; const s: String): TSchemaCIOT;
var
  P: Integer;
  SchemaStr: String;
begin
  P := pos('_', s);
  if P > 0 then
    SchemaStr := copy(s, 1, P-1)
  else
    SchemaStr := s;

  if LeftStr(SchemaStr, 3) <> 'sch' then
    SchemaStr := 'sch' + SchemaStr;

  Result := TSchemaCIOT( GetEnumValue(TypeInfo(TSchemaCIOT), SchemaStr ) );
end;

function VersaoCIOTToStr(const t: TVersaoCIOT): String;
begin
  Result := EnumeradoToStr(t, ['5.00'], [ve500]);
end;

function StrToVersaoCIOT(out ok: Boolean; const s: String): TVersaoCIOT;
begin
  Result := StrToEnumerado(ok, s, ['5.00'], [ve500]);
end;

function VersaoCIOTToInt(const t: TVersaoCIOT): Integer;
begin
  case t of
    ve500: Result := 5;
  else
    Result := 0;
  end;
end;

function VersaoCIOTToDbl(const t: TVersaoCIOT): Double;
begin
  case t of
    ve500: Result := 5;
  else
    Result := 0;
  end;
end;

function DblToVersaoCIOT(out ok: Boolean; const d: Double): TVersaoCIOT;
begin
  ok := True;

  if d = 5.0 then
    Result := ve500
  else
  begin
    Result := ve500;
    ok := False;
  end;
end;

function TipoContaToStr(const t: tpTipoConta): string;
begin
  Result := EnumeradoToStr(t, ['ContaCorrente', 'ContaPoupanca', 'ContaPagamentos'],
                          [tcContaCorrente, tcContaPoupanca, tcContaPagamentos]);
end;

function TipoEmbalagemToStr(const t: tpTipoEmbalagem): string;
begin
  Result := EnumeradoToStr(t, ['Bigbag', 'Pallet', 'Granel', 'Container', 'Saco'],
                          [teBigbag, tePallet, teGranel, teContainer, teSaco]);
end;

function StrToTipoEmbalagem(out ok: Boolean; const s: String): tpTipoEmbalagem;
begin
  Result := StrToEnumerado(ok, s, ['Bigbag', 'Pallet', 'Granel', 'Container', 'Saco'],
                          [teBigbag, tePallet, teGranel, teContainer, teSaco]);
end;

function TipoViagemCIOTToStr(const t: TpTipoViagem): String;
begin
  Result := EnumeradoToStr(t, ['Padrao', 'TAC_Agregado', 'Frota'],
                              [Padrao, TAC_Agregado, Frota]);
end;

function TpPagamentoToStr(const t: TpTipoPagamento): string;
begin
  result := EnumeradoToStr(t, ['TransferenciaBancaria', 'eFRETE'],
                              [TransferenciaBancaria, eFRETE]);
end;

function StrToTpPagamento(out ok: Boolean; const s: String): TpTipoPagamento;
begin
  Result := StrToEnumerado(ok, s, ['TransferenciaBancaria', 'eFRETE'],
                                  [TransferenciaBancaria, eFRETE]);
end;

function TpUnMedMercToStr(const t: TpUnidadeDeMedidaDaMercadoria): string;
begin
  result := EnumeradoToStr(t, ['Tonelada', 'Kg'],
                              [umTonelada, umKg]);
end;

function TpVgTipoCalculoToStr(const t: TpViagemTipoDeCalculo): string;
begin
  result := EnumeradoToStr(t, ['SemQuebra', 'QuebraSomenteUltrapassado', 'QuebraIntegral'],
                              [SemQuebra, QuebraSomenteUltrapassado, QuebraIntegral]);
end;

function TpProporcaoToStr(const t: TpTipoProporcao): string;
begin
  result := EnumeradoToStr(t, ['Nenhum', 'Porcentagem', 'ValorAbsoluto'],
                              [tpNenhum, tpPorcentagem, tpValorAbsoluto]);
end;

function StrToTpProporcao(out ok: Boolean; const s: String): TpTipoProporcao;
begin
  Result := StrToEnumerado(ok, s, ['Nenhum', 'Porcentagem', 'ValorAbsoluto'],
                                  [tpNenhum, tpPorcentagem, tpValorAbsoluto]);
end;

function TpDifFreteToStr(const t: TpDiferencaFrete): string;
begin
  result := EnumeradoToStr(t,['SemDiferenca', 'SomenteUltrapassado', 'Integral'],
                             [SemDiferenca, SomenteUltrapassado, Integral]);
end;

function StrToTpDifFrete(out ok: Boolean; const s: String): TpDiferencaFrete;
begin
  Result := StrToEnumerado(ok, s, ['SemDiferenca', 'SomenteUltrapassado', 'Integral'],
                                  [SemDiferenca, SomenteUltrapassado, Integral]);
end;

function TpDiferencaFreteBCToStr(const t: TpDiferencaFreteBaseCalculo): string;
begin
  result := EnumeradoToStr(t, ['QuantidadeDesembarque', 'QuantidadeMenor'],
                              [QuantidadeDesembarque, QuantidadeMenor]);
end;

function StrToTpDiferencaFreteBC(out ok: Boolean; const s: String): TpDiferencaFreteBaseCalculo;
begin
  Result := StrToEnumerado(ok, s, ['QuantidadeDesembarque', 'QuantidadeMenor'],
                              [QuantidadeDesembarque, QuantidadeMenor]);
end;

function TpCatPagToStr(const t: TpTipoCategoriaPagamento): string;
begin
  result := EnumeradoToStr(t, ['Adiantamento', 'Estadia', 'Quitacao', 'SemCategoria', 'Frota'],
                              [tcpAdiantamento, tcpEstadia, tcpQuitacao, tcpSemCategoria, tcpFrota]);
end;

function TpOperacaoToStr(const t: TpOperacao): string;
begin
  Result := EnumeradoToStr(t,
                ['Login', 'Logout', 'Gravar Proprietario',
                'Gravar Veículo', 'Gravar Motorista',
                'Adicionar', 'Adicionar Viagem', 'Adicionar Pagamento',
                'Obter Codigo CIOT', 'Obter Pdf', 'Retificar', 'Cancelar',
                'Cancelar Pagamento', 'Encerrar', 'ConsultarTipoCarga',
                'AlterarDataLiberacaoPagamento'],
                [opLogin, opLogout, opGravarProprietario,
                opGravarVeiculo, opGravarMotorista,
                opAdicionar, opAdicionarViagem, opAdicionarPagamento,
                opObterCodigoIOT, opObterPdf, opRetificar, opCancelar,
                opCancelarPagamento, opEncerrar, opConsultarTipoCarga,
                opAlterarDataLiberacaoPagamento]);
end;

function EntregaDocumentacaoToStr(const t: TpEntregaDocumentacao): string;
begin
  result := EnumeradoToStr(t, ['RedeCredenciada', 'Cliente'],
                              [edRedeCredenciada, edCliente]);
end;

function TipoRodadoToStr(const t: TpTipoRodado): string;
begin
  Result := EnumeradoToStr(t, ['NaoAplicavel', 'Truck', 'Toco', 'Cavalo'],
                              [trNaoAplicavel, trTruck, trToco, trCavalo]);
end;

function StrToTipoRodado(out ok: Boolean; const s: String): TpTipoRodado;
begin
  Result := StrToEnumerado(ok, s, ['NaoAplicavel', 'Truck', 'Toco', 'Cavalo'],
                                  [trNaoAplicavel, trTruck, trToco, trCavalo]);
end;

function TipoCarroceriaToStr(const t: TpTipoCarroceria): string;
begin
  Result := EnumeradoToStr(t, ['NaoAplicavel', 'Aberta', 'FechadaOuBau',
                               'Granelera', 'PortaContainer', 'Sider'],
                         [tcNaoAplicavel, tcAberta, tcFechadaOuBau, tcGranelera,
                          tcPortaContainer, tcSider]);
end;

function StrToTipoCarroceria(out ok: Boolean; const s: String): TpTipoCarroceria;
begin
  Result := StrToEnumerado(ok, s, ['NaoAplicavel', 'Aberta', 'FechadaOuBau',
                                   'Granelera', 'PortaContainer', 'Sider'],
                         [tcNaoAplicavel, tcAberta, tcFechadaOuBau, tcGranelera,
                          tcPortaContainer, tcSider]);
end;

function TipoPessoaToStr(const t: tpTipoPessoa): string;
begin
  Result := EnumeradoToStr(t, ['Indefinido', 'Fisica', 'Juridica'],
                              [tpIndefinido, tpFisica, tpJuridica]);
end;

function StrToTipoPessoa(out ok: Boolean; const s: String): tpTipoPessoa;
begin
  Result := StrToEnumerado(ok, s, ['Indefinido', 'Fisica', 'Juridica'],
                                  [tpIndefinido, tpFisica, tpJuridica]);
end;

function TipoProprietarioToStr(const t: TpTipoProprietario): string;
begin
  Result := EnumeradoToStr(t, ['TAC', 'ETC', 'CTC'],
                              [tpTAC, tpETC, tpCTC]);
end;

function StrToTipoProprietario(out ok: Boolean; const s: String): TpTipoProprietario;
begin
  Result := StrToEnumerado(ok, s, ['TAC', 'ETC', 'CTC'],
                                  [tpTAC, tpETC, tpCTC]);
end;

function EstadoCIOTToStr(const t: TpEstadoCIOT): string;
begin
  Result := EnumeradoToStr(t, ['EmViagem', 'Encerrado', 'Cancelado'],
                              [ecEmViagem, ecEncerrado, ecCancelado]);
end;

function StrToEstadoCIOT(out ok: Boolean; const s: String): TpEstadoCIOT;
begin
  Result := StrToEnumerado(ok, s, ['EmViagem', 'Encerrado', 'Cancelado'],
                                  [ecEmViagem, ecEncerrado, ecCancelado]);
end;

function TipoCargaToStr(const t: tpTipoCarga): string;
begin
  Result := EnumeradoToStr(t, ['Granel sólido - HML', 'Granel líquido - HML',
                               'Frigorificada - HML', 'Conteinerizada - HML',
                               'Carga Geral - HML', 'Neogranel - HML',
                               'Perigosa (granel sólido) - HML',
                    					 'Perigosa (granel líquido) - HML',
                               'Perigosa (carga frigorificada) - HML',
                               'Perigosa (conteinerizada) - HML',
                               'Perigosa (carga geral) - HML'],
                              [tpGranelsolido, tpGranelLiquido, tpFrigorificada,
                               tpConteinerizada, tpCargaGeral, tpNeogranel,
                               tpPerigosaGranelSolido, tpPerigosaGranelLiquido,
                               tpPerigosaCargaFrigorificada,
                               tpPerigosaConteinerizada, tpPerigosaCargaGeral]);
end;

function StrToTipoCarga(out ok: Boolean; const s: String): tpTipoCarga;
begin
  Result := StrToEnumerado(ok, s, ['Granel sólido - HML', 'Granel líquido - HML',
                                   'Frigorificada - HML', 'Conteinerizada - HML',
                                   'Carga Geral - HML', 'Neogranel - HML',
                                   'Perigosa (granel sólido) - HML',
                                   'Perigosa (granel líquido) - HML',
                                   'Perigosa (carga frigorificada) - HML',
                                   'Perigosa (conteinerizada) - HML',
                                   'Perigosa (carga geral) - HML'],
                              [tpGranelsolido, tpGranelLiquido, tpFrigorificada,
                               tpConteinerizada, tpCargaGeral, tpNeogranel,
                               tpPerigosaGranelSolido, tpPerigosaGranelLiquido,
                               tpPerigosaCargaFrigorificada,
                               tpPerigosaConteinerizada, tpPerigosaCargaGeral]);
end;

function StrToEnumIntegradora(out ok: Boolean; const s: String): TCIOTIntegradora;
begin
  Result := StrToEnumerado(ok, s,
                          ['ieFrete', 'iRepom', 'iPamcard'],
                          [ieFrete, iRepom, iPamcard] );
end;

end.
