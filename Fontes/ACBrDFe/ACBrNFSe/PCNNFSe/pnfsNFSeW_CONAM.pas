{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
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

{$I ACBr.inc}

unit pnfsNFSeW_CONAM;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrConsts,
  pnfsNFSeW, pcnAuxiliar, pcnConversao, pcnGerador,
  pnfsNFSe, pnfsConversao, pnfsConsts;

type
  { TNFSeW_CONAM }

  TNFSeW_CONAM = class(TNFSeWClass)
  private
    FQtdReg30: Integer;
    FValReg30: Real;
  protected
    procedure GerarIdentificacaoRPS;
    procedure GerarListaServicos;
    procedure GeraTributos;
    procedure GerarValoresServico;
    procedure GerarXML_CONAM;
  public
    constructor Create(ANFSeW: TNFSeW); override;
    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;

    property QtdReg30: Integer read FQtdReg30 write FQtdReg30;
    property ValReg30: Real read FValReg30 write FValReg30;
  end;

implementation

uses
  ACBrUtil;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout do CONAM.                                                             }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_CONAM }

constructor TNFSeW_CONAM.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

function TNFSeW_CONAM.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

procedure TNFSeW_CONAM.GerarIdentificacaoRPS;
begin
  Gerador.wCampoNFSe(tcStr, '', 'Ano'    , 01, 04, 0, FormatDateTime('yyyy', FNFSe.DataEmissaoRps) , '');
  Gerador.wCampoNFSe(tcStr, '', 'Mes'    , 01, 02, 0, FormatDateTime('mm', FNFSe.DataEmissaoRps) , '');
  Gerador.wCampoNFSe(tcStr, '', 'CPFCNPJ', 01, 14, 0, FNFSe.Prestador.Cnpj , '');
  Gerador.wCampoNFSe(tcStr, '', 'DTIni'  , 01, 10, 0, FormatDateTime('dd/mm/yyyy', FNFSe.DataEmissaoRps) , '');
  Gerador.wCampoNFSe(tcStr, '', 'DTFin'  , 01, 10, 0, FormatDateTime('dd/mm/yyyy', FNFSe.DataEmissaoRps) , '');

  if FNFSe.OptanteSimplesNacional = snSim then
  begin
    Gerador.wCampoNFSe(tcInt, '', 'TipoTrib'   , 01, 01, 0, 4 , '');
    Gerador.wCampoNFSe(tcStr, '', 'DtAdeSN'    , 01, 10, 0, FormatDateTime('dd/mm/yyyy', NFSe.DataOptanteSimplesNacional) , ''); //data de adesao ao simples nacional
    Gerador.wCampoNFSe(tcDe2, '', 'AlqIssSN_IP', 01, 06, 0, NFSe.ValoresNfse.Aliquota, '');
  end
  else begin
    case FNFSe.Servico.ExigibilidadeISS of
      exiExigivel:                       Gerador.wCampoNFSe(tcInt, '', 'TipoTrib', 001, 1, 0, 1 , '');
      exiNaoIncidencia:                  Gerador.wCampoNFSe(tcInt, '', 'TipoTrib', 001, 1, 0, 2 , '');
      exiIsencao:                        Gerador.wCampoNFSe(tcInt, '', 'TipoTrib', 001, 1, 0, 2 , '');
      exiExportacao:                     Gerador.wCampoNFSe(tcInt, '', 'TipoTrib', 001, 1, 0, 5 , '');
      exiImunidade:                      Gerador.wCampoNFSe(tcInt, '', 'TipoTrib', 001, 1, 0, 2 , '');
      exiSuspensaDecisaoJudicial:        Gerador.wCampoNFSe(tcInt, '', 'TipoTrib', 001, 1, 0, 3 , '');
      exiSuspensaProcessoAdministrativo: Gerador.wCampoNFSe(tcInt, '', 'TipoTrib', 001, 1, 0, 3 , '');
    end;

    Gerador.wCampoNFSe(tcStr, '', 'DtAdeSN'    , 01, 10, 0, '', ''); //data de adesao ao simples nacional
    Gerador.wCampoNFSe(tcStr, '', 'AlqIssSN_IP', 01, 06, 0, '' , '');
  end;

  if FNFSe.RegimeEspecialTributacao = retMicroempresarioIndividual then
    Gerador.wCampoNFSe(tcStr, '', 'AlqIssSN_IP', 001, 6, 0, '' , '');

  Gerador.wCampoNFSe(tcStr, '', 'Versao', 001, 4, 0, '2.00' , '');
end;

procedure TNFSeW_CONAM.GerarListaServicos;
var
  i: Integer;
  CpfCnpj: String;
  MunTomador: String;
  MunPrestador: String;
begin
  //Gerador.wGrupoNFSe('Reg20');

  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupoNFSe('Reg20Item');
  if FNFSe.IdentificacaoRps.Tipo = trRPS then
    Gerador.wCampoNFSe(tcStr, '', 'TipoNFS', 01, 3, 1, 'RPS' , '')
  else
    Gerador.wCampoNFSe(tcStr, '', 'TipoNFS', 01, 3, 1, 'RPC' , '');

  Gerador.wCampoNFSe(tcStr, '', 'NumRps', 01, 9, 1, FNFSe.IdentificacaoRps.Numero , DSC_NUMRPS);

  if NFSe.IdentificacaoRps.Serie = '' then
    Gerador.wCampoNFSe(tcStr, '', 'SerRps', 01, 03, 1, '001', DSC_SERIERPS)
  else
    Gerador.wCampoNFSe(tcStr, '', 'SerRps', 01, 03, 1, NFSe.IdentificacaoRps.Serie, DSC_SERIERPS);

  Gerador.wCampoNFSe(tcStr, '', 'DtEmi', 01, 10, 1, FormatDateTime('dd/mm/yyyy',NFse.DataEmissaoRps), '');

  if NFSe.Servico.Valores.IssRetido = stNormal then
    Gerador.wCampoNFSe(tcStr, '', 'RetFonte', 01, 03, 1, 'NAO', '')
  else
    Gerador.wCampoNFSe(tcStr, '', 'RetFonte', 01, 03, 1, 'SIM', '');

  Gerador.wCampoNFSe(tcStr, '', 'CodSrv'   , 01,   05, 1, NFSe.Servico.ItemListaServico, '');
  Gerador.wCampoNFSe(tcStr, '', 'DiscrSrv' , 01, 4000, 1, StringReplace( NFSe.Servico.Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase] ), '');
  Gerador.wCampoNFSe(tcDe2, '', 'VlNFS'    , 01,   16, 2, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampoNFSe(tcDe2, '', 'VlDed'    , 01,   16, 2, NFSe.Servico.Valores.ValorDeducoes, '');
  Gerador.wCampoNFSe(tcStr, '', 'DiscrDed' , 01, 4000, 1,StringReplace( NFSe.Servico.Valores.JustificativaDeducao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase] ), '');
  Gerador.wCampoNFSe(tcDe2, '', 'VlBasCalc', 01,   16, 2, NFSe.Servico.Valores.BaseCalculo, '');
  Gerador.wCampoNFSe(tcDe2, '', 'AlqIss'   , 01,   05, 2, NFSe.Servico.Valores.Aliquota, '');
  Gerador.wCampoNFSe(tcDe2, '', 'VlIss'    , 01,   16, 2, NFSe.Servico.Valores.ValorIss, '');
  Gerador.wCampoNFSe(tcDe2, '', 'VlIssRet' , 01,   16, 2, NFSe.Servico.Valores.ValorIssRetido, '');

  CpfCnpj := UpperCase(StringReplace(StringReplace(StringReplace(NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '.', '', [rfReplaceAll]), '-', '', [rfReplaceAll]), '/', '', [rfReplaceAll]));

  {Todo: por : almp1
    Se hover municipios com apostofro deve ser substituido por espaço,
    por exemplo SANTA BARBARA D'OESTE
    deve informar SANTA BARBARA D OESTE
  }
  MunPrestador:= UpperCase(StringReplace(NFSe.PrestadorServico.Endereco.xMunicipio, '''', ' ', [rfReplaceAll]));
  MunTomador:= UpperCase(StringReplace(NFSe.Tomador.Endereco.xMunicipio, '''', ' ', [rfReplaceAll]));

  Gerador.wCampoNFSe(tcStr, '', 'CpfCnpTom'  , 01, 14, 1, CpfCnpj, '');
  Gerador.wCampoNFSe(tcStr, '', 'RazSocTom'  , 01, 60, 1, NFSe.Tomador.RazaoSocial, '');
  Gerador.wCampoNFSe(tcStr, '', 'TipoLogtom' , 01, 10, 1, NFSe.Tomador.Endereco.TipoLogradouro, '');
  Gerador.wCampoNFSe(tcStr, '', 'LogTom'     , 01, 60, 1, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampoNFSe(tcStr, '', 'NumEndTom'  , 01, 10, 1, NFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampoNFSe(tcStr, '', 'ComplEndTom', 01, 60, 0, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampoNFSe(tcStr, '', 'BairroTom'  , 01, 60, 1, NFSe.Tomador.Endereco.Bairro, '');

  if CpfCnpj='CONSUMIDOR'  then
   begin
    Gerador.wCampoNFSe(tcStr, '', 'MunTom'    , 01, 60, 1, MunPrestador, '');
    Gerador.wCampoNFSe(tcStr, '', 'SiglaUFTom', 01, 02, 1, NFSe.PrestadorServico.Endereco.UF, '');
   end
  else
   begin
    Gerador.wCampoNFSe(tcStr, '', 'MunTom'    , 01, 60, 1, MunTomador, '');
    Gerador.wCampoNFSe(tcStr, '', 'SiglaUFTom', 01, 02, 1, NFSe.Tomador.Endereco.UF, '');
   end;

  Gerador.wCampoNFSe(tcStr, '', 'CepTom'            , 01, 08, 1, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
  Gerador.wCampoNFSe(tcStr, '', 'Telefone'          , 00, 10, 1, RightStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),8), '');
  Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipal', 01, 20, 1, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');

  {Todo: por : mauroasl
   Se o local que for prestado o serviço for no endereço do tomador, então preencher com os dados do tomador,
  caso contratio, preencher com os dados do prestador do serviço.
  Exemplo pratico é um estacionamento, quando é solicitado uma nfse de serviço. Serviço prestado no prestador.
  }

  Gerador.wCampoNFSe(tcStr, '', 'TipoLogLocPre' , 01, 10, 1, IfThen(NFSe.LogradouLocalPrestacaoServico = llpTomador, NFSe.Tomador.Endereco.TipoLogradouro, NFSe.PrestadorServico.Endereco.TipoLogradouro), '');
  Gerador.wCampoNFSe(tcStr, '', 'LogLocPre'     , 01, 60, 1, IfThen(NFSe.LogradouLocalPrestacaoServico = llpTomador, NFSe.Tomador.Endereco.Endereco, NFSe.PrestadorServico.Endereco.Endereco), '');
  Gerador.wCampoNFSe(tcStr, '', 'NumEndLocPre'  , 01, 10, 1, IfThen(NFSe.LogradouLocalPrestacaoServico = llpTomador, NFSe.Tomador.Endereco.Numero, NFSe.PrestadorServico.Endereco.Numero), '');
  Gerador.wCampoNFSe(tcStr, '', 'ComplEndLocPre', 01, 60, 0, IfThen(NFSe.LogradouLocalPrestacaoServico = llpTomador, NFSe.Tomador.Endereco.Complemento, NFSe.PrestadorServico.Endereco.Complemento), '');
  Gerador.wCampoNFSe(tcStr, '', 'BairroLocPre'  , 01, 60, 1, IfThen(NFSe.LogradouLocalPrestacaoServico = llpTomador, NFSe.Tomador.Endereco.Bairro, NFSe.PrestadorServico.Endereco.Bairro), '');
  Gerador.wCampoNFSe(tcStr, '', 'MunLocPre'     , 01, 60, 1, IfThen(NFSe.LogradouLocalPrestacaoServico = llpTomador, MunTomador, MunPrestador), '');
  Gerador.wCampoNFSe(tcStr, '', 'SiglaUFLocpre' , 01, 02, 1, IfThen(NFSe.LogradouLocalPrestacaoServico = llpTomador, NFSe.Tomador.Endereco.UF, NFSe.PrestadorServico.Endereco.UF), '');
  Gerador.wCampoNFSe(tcStr, '', 'CepLocPre'     , 01, 08, 1, IfThen(NFSe.LogradouLocalPrestacaoServico = llpTomador, OnlyNumber(NFSe.Tomador.Endereco.CEP), OnlyNumber(NFSe.PrestadorServico.Endereco.CEP)), '');

  Gerador.wCampoNFSe(tcStr, '', 'Email1', 01, 120, 1, NFSe.Tomador.Contato.Email, '');

  for i:= 0 to NFSe.email.Count - 1 do
    Gerador.wCampoNFSe(tcStr, '', 'Email' + IntToStr(i+2), 01, 120,  1, NFSe.email.Items[i].emailCC, '');

  {Todo: por : almp1
    caso tenham tributos, estes podem ser declarados no registro 30 do XML
    assim eles serao destacados de forma separada na impressao da NFse
  }
  //So gera se houver tributos declarados
  if (NFSe.Servico.Valores.AliquotaPis > 0) or
     (NFSe.Servico.Valores.AliquotaCofins > 0) or
     (NFSe.Servico.Valores.AliquotaCsll > 0) or
     (NFSe.Servico.Valores.AliquotaInss > 0) or
     (NFSe.Servico.Valores.AliquotaIr > 0) then
     GeraTributos;

  Gerador.wGrupoNFSe('/Reg20Item');
  //Gerador.wGrupoNFSe('/Reg20');
end;

procedure TNFSeW_CONAM.GeraTributos;
begin
  (*
  Contém os tributos municipais, Estaduais e Federais que devem ser
  destacados na nota fiscal eletrônica impressa.
  Siglas de tributos permitidas:
  COFINS
  CSLL
  INSS
  IR
  PIS
*)
  QtdReg30:=0;
  ValReg30:=0;
  Gerador.wGrupoNFSe('Reg30');

  if NFSe.Servico.Valores.AliquotaPis > 0 then
    begin
      Gerador.wGrupoNFSe('Reg30Item');
      Gerador.wCampoNFSe(tcStr, '', 'TributoSigla'   , 01,  06, 1, 'PIS', '');
      Gerador.wCampoNFSe(tcDe2, '', 'TributoAliquota', 01,  05, 2, NFSe.Servico.Valores.AliquotaPis, '');
      Gerador.wCampoNFSe(tcDe2, '', 'TributoValor'   , 01,  16, 2, NFSe.Servico.Valores.ValorPis, '');
      Gerador.wGrupoNFSe('/Reg30Item');
      QtdReg30:=QtdReg30+1;
      ValReg30:=ValReg30+NFSe.Servico.Valores.ValorPis;
    end;

  if NFSe.Servico.Valores.AliquotaCofins > 0 then
    begin
      Gerador.wGrupoNFSe('Reg30Item');
      Gerador.wCampoNFSe(tcStr, '', 'TributoSigla'   , 01,  06, 1, 'COFINS', '');
      Gerador.wCampoNFSe(tcDe2, '', 'TributoAliquota', 01,  05, 2, NFSe.Servico.Valores.AliquotaCofins, '');
      Gerador.wCampoNFSe(tcDe2, '', 'TributoValor'   , 01,  16, 2, NFSe.Servico.Valores.ValorCofins, '');
      Gerador.wGrupoNFSe('/Reg30Item');
      QtdReg30:=QtdReg30+1;
      ValReg30:=ValReg30+NFSe.Servico.Valores.ValorCofins;
    end;

  if NFSe.Servico.Valores.AliquotaCsll > 0 then
    begin
      Gerador.wGrupoNFSe('Reg30Item');
      Gerador.wCampoNFSe(tcStr, '', 'TributoSigla'   , 01,  06, 1, 'CSLL', '');
      Gerador.wCampoNFSe(tcDe2, '', 'TributoAliquota', 01,  05, 2, NFSe.Servico.Valores.AliquotaCsll, '');
      Gerador.wCampoNFSe(tcDe2, '', 'TributoValor'   , 01,  16, 2, NFSe.Servico.Valores.ValorCsll, '');
      Gerador.wGrupoNFSe('/Reg30Item');
      QtdReg30:=QtdReg30+1;
      ValReg30:=ValReg30+NFSe.Servico.Valores.ValorCsll;
    end;

  if NFSe.Servico.Valores.AliquotaInss > 0 then
    begin
      Gerador.wGrupoNFSe('Reg30Item');
      Gerador.wCampoNFSe(tcStr, '', 'TributoSigla'   , 01,  06, 1, 'INSS', '');
      Gerador.wCampoNFSe(tcDe2, '', 'TributoAliquota', 01,  05, 2, NFSe.Servico.Valores.AliquotaInss, '');
      Gerador.wCampoNFSe(tcDe2, '', 'TributoValor'   , 01,  16, 2, NFSe.Servico.Valores.ValorInss, '');
      Gerador.wGrupoNFSe('/Reg30Item');
      QtdReg30:=QtdReg30+1;
      ValReg30:=ValReg30+NFSe.Servico.Valores.ValorInss;
    end;

  if NFSe.Servico.Valores.AliquotaIr > 0 then
    begin
      Gerador.wGrupoNFSe('Reg30Item');
      Gerador.wCampoNFSe(tcStr, '', 'TributoSigla'   , 01,  06, 1, 'IR', '');
      Gerador.wCampoNFSe(tcDe2, '', 'TributoAliquota', 01,  05, 2, NFSe.Servico.Valores.AliquotaIr, '');
      Gerador.wCampoNFSe(tcDe2, '', 'TributoValor'   , 01,  16, 2, NFSe.Servico.Valores.ValorIr, '');
      Gerador.wGrupoNFSe('/Reg30Item');
      QtdReg30:=QtdReg30+1;
      ValReg30:=ValReg30+NFSe.Servico.Valores.ValorIr;
    end;

  Gerador.wGrupoNFSe('/Reg30');

end;

procedure TNFSeW_CONAM.GerarValoresServico;
begin
  Gerador.wGrupoNFSe('Reg90');
  Gerador.wCampoNFSe(tcStr, '', 'QtdRegNormal'  , 01, 05, 1, '1', '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorNFS'      , 01, 16, 2, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorISS'      , 01, 16, 2, NFSe.Servico.Valores.ValorIss, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorDed'      , 01, 16, 2, NFSe.Servico.Valores.ValorDeducoes, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorIssRetTom', 01, 16, 2, NFSe.Servico.Valores.ValorIssRetido, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorTributos' , 01, 16, 2, ValReg30, '');
  Gerador.wCampoNFSe(tcStr, '', 'QtdReg30'      , 01, 05, 1, QtdReg30, '');

  Gerador.wGrupoNFSe('/Reg90');
end;

function TNFSeW_CONAM.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;
  GerarXML_CONAM;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TNFSeW_CONAM.GerarXML_CONAM;
begin
(*
  Gerador.Opcoes.RetirarEspacos := False;
  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;
*)
  //Gerador.Opcoes.IdentarXML:=True;

  //Gerador.wGrupoNFSe('SDTRPS');
  //GerarIdentificacaoRPS;
  Gerador.Opcoes.DecimalChar := ',';
  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  GerarListaServicos;
  //GerarValoresServico;
  //Gerador.wGrupoNFSe('/SDTRPS');
end;

end.

