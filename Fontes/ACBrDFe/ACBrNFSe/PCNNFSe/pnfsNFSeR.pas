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

unit pnfsNFSeR;

interface

uses
  SysUtils, Classes, Forms, DateUtils,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnAuxiliar, pcnConversao, pcnLeitor, pnfsNFSe, pnfsConversao,
  ACBrUtil, ACBrNFSeUtil, ACBrDFeUtil;

type

 TLeitorOpcoes   = class;

 TNFSeR = class(TPersistent)
  private
    FLeitor: TLeitor;
    FNFSe: TNFSe;
    FOpcoes: TLeitorOpcoes;
    FVersaoXML: String;
    FProvedor: TnfseProvedor;
    FTabServicosExt: Boolean;

    function LerRPS_ABRASF_V1: Boolean;
    function LerRPS_ABRASF_V2: Boolean;
    function LerRPS_IssDSF: Boolean;
    function LerRPS_Equiplano: Boolean;

    function LerNFSe_ABRASF_V1: Boolean;
    function LerNFSe_ABRASF_V2: Boolean;
    function LerNFSe_IssDSF: Boolean;
    function LerNFSe_Infisc: Boolean;
    function LerNFSe_Equiplano: Boolean;
    function LerNFSe_ProvedorSysPro: Boolean;

    function LerRPS: Boolean;
    function LerNFSe: Boolean;
  public
    constructor Create(AOwner: TNFSe);
    destructor Destroy; override;
    function LerXml: boolean;
  published
    property Leitor: TLeitor         read FLeitor         write FLeitor;
    property NFSe: TNFSe             read FNFSe           write FNFSe;
    property Opcoes: TLeitorOpcoes   read FOpcoes         write FOpcoes;
    property VersaoXML: String       read FVersaoXML      write FVersaoXML;
    property Provedor: TnfseProvedor read FProvedor       write FProvedor;
    property TabServicosExt: Boolean read FTabServicosExt write FTabServicosExt;
  end;

 TLeitorOpcoes = class(TPersistent)
  private
    FPathArquivoMunicipios: string;
    FPathArquivoTabServicos: string;
  published
    property PathArquivoMunicipios: string read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property PathArquivoTabServicos: string read FPathArquivoTabServicos write FPathArquivoTabServicos;
  end;

implementation

{ TNFSeR }

constructor TNFSeR.Create(AOwner: TNFSe);
begin
 FLeitor := TLeitor.Create;
 FNFSe   := AOwner;
 FOpcoes := TLeitorOpcoes.Create;
 FOpcoes.FPathArquivoMunicipios  := '';
 FOpcoes.FPathArquivoTabServicos := '';
end;

destructor TNFSeR.Destroy;
begin
 FLeitor.Free;
 FOpcoes.Free;

 inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

function TNFSeR.LerRPS_ABRASF_V1: Boolean;
var
 item, i: Integer;
 ok  : Boolean;
begin
 if (Leitor.rExtrai(2, 'InfRps') <> '') or (Leitor.rExtrai(1, 'Rps') <> '')
  then begin
   NFSe.DataEmissaoRps           := Leitor.rCampo(tcDat, 'DataEmissao');

   if (Leitor.rExtrai(1, 'InfRps') <> '')
    then NFSe.DataEmissao        := Leitor.rCampo(tcDatHor, 'DataEmissao');

   NFSe.NaturezaOperacao         := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'NaturezaOperacao'));
   NFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
   NFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
   NFSe.IncentivadorCultural     := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivadorCultural'));
   NFSe.Status                   := StrToStatusRPS(ok, Leitor.rCampo(tcStr, 'Status'));
   NFSe.OutrasInformacoes        := Leitor.rCampo(tcStr, 'OutrasInformacoes');

   if (Leitor.rExtrai(3, 'IdentificacaoRps') <> '') or (Leitor.rExtrai(2, 'IdentificacaoRps') <> '')
    then begin
     NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
     NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
     NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
     NFSe.InfID.ID                := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
    end;

   if (Leitor.rExtrai(3, 'RpsSubstituido') <> '') or (Leitor.rExtrai(2, 'RpsSubstituido') <> '')
    then begin
     NFSe.RpsSubstituido.Numero := Leitor.rCampo(tcStr, 'Numero');
     NFSe.RpsSubstituido.Serie  := Leitor.rCampo(tcStr, 'Serie');
     NFSe.RpsSubstituido.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
    end;

   if (Leitor.rExtrai(3, 'Servico') <> '') or (Leitor.rExtrai(2, 'Servico') <> '')
    then begin
     NFSe.Servico.ItemListaServico          := OnlyNumber(Leitor.rCampo(tcStr, 'ItemListaServico'));

     // ALTERTADO POR TÚLIO DAPPER EM 25/03
     NFSe.Servico.CodigoCnae                := Leitor.rCampo(tcStr, 'CodigoCnae');

     NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio');
     NFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'Discriminacao');
     NFSe.Servico.Descricao                 := '';

     if VersaoXML = '1'
      then NFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'MunicipioPrestacaoServico')
      else NFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');

     Item := StrToIntDef(OnlyNumber(Nfse.Servico.ItemListaServico), 0);
     if Item<100 then Item:=Item*100+1;

     NFSe.Servico.ItemListaServico := FormatFloat('0000', Item);

     if FProvedor <> ProRJ then
       NFSe.Servico.ItemListaServico := Copy(NFSe.Servico.ItemListaServico, 1, 2) + '.' +
                                        Copy(NFSe.Servico.ItemListaServico, 3, 2);

     if TabServicosExt
      then NFSe.Servico.xItemListaServico := NotaUtil.ObterDescricaoServico(OnlyNumber(NFSe.Servico.ItemListaServico))
      else NFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(NFSe.Servico.ItemListaServico));

     if length(NFSe.Servico.CodigoMunicipio) < 7
      then NFSe.Servico.CodigoMunicipio := Copy(NFSe.Servico.CodigoMunicipio, 1, 2) +
            FormatFloat('00000', StrToIntDef(Copy(NFSe.Servico.CodigoMunicipio, 3, 5), 0));

     if (Leitor.rExtrai(4, 'Valores') <> '') or (Leitor.rExtrai(3, 'Valores') <> '')
      then begin
       NFSe.Servico.Valores.ValorServicos          := Leitor.rCampo(tcDe2, 'ValorServicos');
       NFSe.Servico.Valores.ValorDeducoes          := Leitor.rCampo(tcDe2, 'ValorDeducoes');
       NFSe.Servico.Valores.ValorPis               := Leitor.rCampo(tcDe2, 'ValorPis');
       NFSe.Servico.Valores.ValorCofins            := Leitor.rCampo(tcDe2, 'ValorCofins');
       NFSe.Servico.Valores.ValorInss              := Leitor.rCampo(tcDe2, 'ValorInss');
       NFSe.Servico.Valores.ValorIr                := Leitor.rCampo(tcDe2, 'ValorIr');
       NFSe.Servico.Valores.ValorCsll              := Leitor.rCampo(tcDe2, 'ValorCsll');
       NFSe.Servico.Valores.IssRetido              := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
       NFSe.Servico.Valores.ValorIss               := Leitor.rCampo(tcDe2, 'ValorIss');
       NFSe.Servico.Valores.OutrasRetencoes        := Leitor.rCampo(tcDe2, 'OutrasRetencoes');
       NFSe.Servico.Valores.BaseCalculo            := Leitor.rCampo(tcDe2, 'BaseCalculo');
       NFSe.Servico.Valores.Aliquota               := Leitor.rCampo(tcDe3, 'Aliquota');
       NFSe.Servico.Valores.ValorLiquidoNfse       := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');
       NFSe.Servico.Valores.ValorIssRetido         := Leitor.rCampo(tcDe2, 'ValorIssRetido');
       NFSe.Servico.Valores.DescontoCondicionado   := Leitor.rCampo(tcDe2, 'DescontoCondicionado');
       NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'DescontoIncondicionado');
      end;
      
      //Provedor SimplISS permite varios itens servico
      if FProvedor = proSimplISS then
      begin
        i := 1;
        while (Leitor.rExtrai(4, 'ItensServico', 'ItensServico', i) <> '') do
        begin
          with NFSe.Servico.ItemServico.Add do
          begin
            Descricao := Leitor.rCampo(tcStr, 'Descricao');
//            Quantidade := Leitor.rCampo(tcInt, 'Quantidade');
            Quantidade := Leitor.rCampo(tcDe2, 'Quantidade');
            ValorUnitario := Leitor.rCampo(tcDe2, 'ValorUnitario');
          end;
          i := i + 1;
        end;
      end;
    end; // fim Servico

   if (Leitor.rExtrai(3, 'Prestador') <> '') or (Leitor.rExtrai(2, 'Prestador') <> '')
    then begin
     NFSe.Prestador.Cnpj               := Leitor.rCampo(tcStr, 'Cnpj');
     NFSe.Prestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
    end; // fim Prestador

   if (Leitor.rExtrai(3, 'Tomador') <> '') or (Leitor.rExtrai(2, 'Tomador') <> '')
    then begin
     NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');

     NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
     if Copy(NFSe.Tomador.Endereco.Endereco, 1, 10) = '<Endereco>'
      then NFSe.Tomador.Endereco.Endereco := Copy(NFSe.Tomador.Endereco.Endereco, 11, 125);

     NFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
     NFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
     NFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

     if VersaoXML = '1'
      then begin
       NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
       NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
      end
      else begin
       NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
       NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
      end;

     NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

     if length(NFSe.Tomador.Endereco.CodigoMunicipio)<7
      then NFSe.Tomador.Endereco.CodigoMunicipio := Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
            FormatFloat('00000', StrToIntDef(Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

     if NFSe.Tomador.Endereco.UF = ''
      then NFSe.Tomador.Endereco.UF := NFSe.PrestadorServico.Endereco.UF;

     NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));

     if Leitor.rExtrai(4, 'IdentificacaoTomador') <> ''
      then begin
       NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

       if Leitor.rExtrai(5, 'CpfCnpj') <> ''
        then begin
         if Leitor.rCampo(tcStr, 'Cpf')<>''
          then NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
          else NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
        end;
      end;

     if Leitor.rExtrai(4, 'Contato') <> ''
      then begin
       NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
       NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
      end;

    end; // fim Tomador

   if Leitor.rExtrai(3, 'IntermediarioServico') <> ''
    then begin
     NFSe.IntermediarioServico.RazaoSocial        := Leitor.rCampo(tcStr, 'RazaoSocial');
     NFSe.IntermediarioServico.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
     if Leitor.rExtrai(4, 'CpfCnpj') <> ''
      then begin
       if Leitor.rCampo(tcStr, 'Cpf')<>''
        then NFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
        else NFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
      end;
    end;

   if Leitor.rExtrai(3, 'ConstrucaoCivil') <> ''
    then begin
     NFSe.ConstrucaoCivil.CodigoObra := Leitor.rCampo(tcStr, 'CodigoObra');
     NFSe.ConstrucaoCivil.Art        := Leitor.rCampo(tcStr, 'Art');
    end;

  end; // fim InfRps

 Result := True;
end;

function TNFSeR.LerRPS_ABRASF_V2: Boolean;
var
 item, i: Integer;
 ok  : Boolean;
begin
 // Para o provedor ISSDigital
 if (Leitor.rExtrai(2, 'ValoresServico') <> '')
  then begin
   NFSe.Servico.Valores.ValorServicos    := Leitor.rCampo(tcDe2, 'ValorServicos');
   NFSe.Servico.Valores.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');
   NFSe.Servico.Valores.ValorIss         := Leitor.rCampo(tcDe2, 'ValorIss');
  end;

   // Para o provedor ISSDigital
   if (Leitor.rExtrai(2, 'ListaServicos') <> '')
    then begin
     NFSe.Servico.Valores.IssRetido   := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
     NFSe.Servico.ResponsavelRetencao := StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'ResponsavelRetencao'));
     NFSe.Servico.ItemListaServico    := OnlyNumber(Leitor.rCampo(tcStr, 'ItemListaServico'));

     Item := StrToIntDef(OnlyNumber(Nfse.Servico.ItemListaServico), 0);
     if Item<100 then Item:=Item*100+1;

     NFSe.Servico.ItemListaServico := FormatFloat('0000', Item);
     NFSe.Servico.ItemListaServico := Copy(NFSe.Servico.ItemListaServico, 1, 2) + '.' +
                                      Copy(NFSe.Servico.ItemListaServico, 3, 2);

     if TabServicosExt
      then NFSe.Servico.xItemListaServico := NotaUtil.ObterDescricaoServico(OnlyNumber(NFSe.Servico.ItemListaServico))
      else NFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(NFSe.Servico.ItemListaServico));

     //NFSe.Servico.Discriminacao       := Leitor.rCampo(tcStr, 'Discriminacao');
     NFSe.Servico.CodigoMunicipio     := Leitor.rCampo(tcStr, 'CodigoMunicipio');
     NFSe.Servico.CodigoPais          := Leitor.rCampo(tcInt, 'CodigoPais');
     NFSe.Servico.ExigibilidadeISS    := StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'ExigibilidadeISS'));
     NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'MunicipioIncidencia');

     NFSe.Servico.Valores.Aliquota    := Leitor.rCampo(tcDe3, 'Aliquota');

     //Se não me engano o maximo de servicos é 10...não?
     for I := 1 to 10
      do begin
       if (Leitor.rExtrai(2, 'Servico', 'Servico', i) <> '')
        then begin

         with NFSe.Servico.ItemServico.Add
          do begin
           Descricao       := Leitor.rCampo(tcStr, 'Discriminacao');

           if (Leitor.rExtrai(3, 'Valores') <> '')
            then begin
             ValorServicos := Leitor.rCampo(tcDe2, 'ValorServicos');
             ValorDeducoes := Leitor.rCampo(tcDe2, 'ValorDeducoes');
             ValorIss      := Leitor.rCampo(tcDe2, 'ValorIss');
             Aliquota      := Leitor.rCampo(tcDe3, 'Aliquota');
             BaseCalculo   := Leitor.rCampo(tcDe2, 'BaseCalculo');
            end;
         end;
        end else Break;
      end;

    end; // fim lista serviço

 if (Leitor.rExtrai(2, 'InfDeclaracaoPrestacaoServico') <> '') or (Leitor.rExtrai(1, 'InfDeclaracaoPrestacaoServico') <> '')
  then begin
   NFSe.Competencia              := Leitor.rCampo(tcStr, 'Competencia');
   NFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
   NFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
   NFSe.IncentivadorCultural     := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivoFiscal'));
   NFSe.Producao                 := StrToSimNao(ok, Leitor.rCampo(tcStr, 'Producao'));

   if (Leitor.rExtrai(3, 'Rps') <> '') or (Leitor.rExtrai(2, 'Rps') <> '')
    then begin
     NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissao');
     NFSe.DataEmissao := Leitor.rCampo(tcDat, 'DataEmissao');
     NFSe.Status         := StrToStatusRPS(ok, Leitor.rCampo(tcStr, 'Status'));

     if (Leitor.rExtrai(3, 'IdentificacaoRps') <> '')
      then begin
       NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
       NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
       NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
       NFSe.InfID.ID                := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
      end;
    end;

   if (Leitor.rExtrai(3, 'Servico') <> '') or (Leitor.rExtrai(2, 'Servico') <> '')
    then begin
     NFSe.Servico.Valores.IssRetido   := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
     NFSe.Servico.ResponsavelRetencao := StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'ResponsavelRetencao'));
     NFSe.Servico.ItemListaServico    := OnlyNumber(Leitor.rCampo(tcStr, 'ItemListaServico'));
     NFSe.Servico.CodigoCnae          := Leitor.rCampo(tcStr, 'CodigoCnae');

     Item := StrToIntDef(OnlyNumber(Nfse.Servico.ItemListaServico), 0);
     if Item<100 then Item:=Item*100+1;

     NFSe.Servico.ItemListaServico := FormatFloat('0000', Item);
     NFSe.Servico.ItemListaServico := Copy(NFSe.Servico.ItemListaServico, 1, 2) + '.' +
                                      Copy(NFSe.Servico.ItemListaServico, 3, 2);

     if TabServicosExt
      then NFSe.Servico.xItemListaServico := NotaUtil.ObterDescricaoServico(OnlyNumber(NFSe.Servico.ItemListaServico))
      else NFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(NFSe.Servico.ItemListaServico));

     NFSe.Servico.Discriminacao       := Leitor.rCampo(tcStr, 'Discriminacao');
     NFSe.Servico.Descricao           := '';
     NFSe.Servico.CodigoMunicipio     := Leitor.rCampo(tcStr, 'CodigoMunicipio');
     NFSe.Servico.CodigoPais          := Leitor.rCampo(tcInt, 'CodigoPais');
     NFSe.Servico.ExigibilidadeISS    := StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'ExigibilidadeISS'));
     NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'MunicipioIncidencia');

     // Provedor Goiania
     NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio');

     if (Leitor.rExtrai(4, 'Valores') <> '') or (Leitor.rExtrai(3, 'Valores') <> '')
      then begin
       NFSe.Servico.Valores.ValorServicos          := Leitor.rCampo(tcDe2, 'ValorServicos');
       NFSe.Servico.Valores.ValorDeducoes          := Leitor.rCampo(tcDe2, 'ValorDeducoes');
       NFSe.Servico.Valores.ValorPis               := Leitor.rCampo(tcDe2, 'ValorPis');
       NFSe.Servico.Valores.ValorIss               := Leitor.rCampo(tcDe2, 'ValorIss');
       NFSe.Servico.Valores.Aliquota               := Leitor.rCampo(tcDe3, 'Aliquota');
       NFSe.Servico.Valores.OutrasRetencoes        := Leitor.rCampo(tcDe2, 'OutrasRetencoes');

       // Provedor Goiania
       NFSe.Servico.Valores.ValorCofins            := Leitor.rCampo(tcDe2, 'ValorCofins');
       NFSe.Servico.Valores.ValorInss              := Leitor.rCampo(tcDe2, 'ValorInss');
       NFSe.Servico.Valores.ValorIr                := Leitor.rCampo(tcDe2, 'ValorIr');
       NFSe.Servico.Valores.ValorCsll              := Leitor.rCampo(tcDe2, 'ValorCsll');
       NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe3, 'DescontoIncondicionado');
       NFSe.Servico.Valores.DescontoCondicionado   := Leitor.rCampo(tcDe2, 'DescontoCondicionado');

       if (FProvedor = proISSe) then  // Joel Takei 10/12/2014
       begin
         if NFSe.Servico.Valores.IssRetido = stRetencao then
           NFSe.Servico.Valores.ValorIssRetido := Leitor.rCampo(tcDe2, 'ValorIss')
         else
           NFSe.Servico.Valores.ValorIssRetido := 0;
       end
       else
         NFSe.Servico.Valores.ValorIssRetido := Leitor.rCampo(tcDe2, 'ValorIssRetido');

      end;

    end; // fim serviço

   if (Leitor.rExtrai(3, 'Prestador') <> '') or (Leitor.rExtrai(2, 'Prestador') <> '')
    then begin
     NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
     NFSe.Prestador.InscricaoMunicipal := NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;

     if (VersaoXML = '1') or (FProvedor = proDigifred)
      then begin
       if (Leitor.rExtrai(4, 'CpfCnpj') <> '') or (Leitor.rExtrai(3, 'CpfCnpj') <> '')
        then begin
          NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cpf');
          if NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = ''
           then NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
        end;
      end
      else begin
       NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
      end;

      NFSe.Prestador.Cnpj := NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
    end; // fim Prestador

   if (Leitor.rExtrai(3, 'Tomador') <> '') or (Leitor.rExtrai(3, 'TomadorServico') <> '') or
      (Leitor.rExtrai(2, 'Tomador') <> '') or (Leitor.rExtrai(2, 'TomadorServico') <> '')
    then begin
     NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');
     NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual  := Leitor.rCampo(tcStr, 'InscricaoEstadual');

     NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
     if Copy(NFSe.Tomador.Endereco.Endereco, 1, 10) = '<Endereco>'
      then NFSe.Tomador.Endereco.Endereco := Copy(NFSe.Tomador.Endereco.Endereco, 11, 125);

     NFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
     NFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
     NFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

     if VersaoXML = '1'
      then begin
       NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
       NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
      end
      else begin
       NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
       NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
      end;

     NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

     if length(NFSe.Tomador.Endereco.CodigoMunicipio)<7
      then NFSe.Tomador.Endereco.CodigoMunicipio := Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
        FormatFloat('00000', StrToIntDef(Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

     if NFSe.Tomador.Endereco.UF = ''
      then NFSe.Tomador.Endereco.UF := NFSe.PrestadorServico.Endereco.UF;

     NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));

     if (Leitor.rExtrai(4, 'IdentificacaoTomador') <> '') or (Leitor.rExtrai(3, 'IdentificacaoTomador') <> '')
      then begin
       NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

       if (Leitor.rExtrai(5, 'CpfCnpj') <> '') or (Leitor.rExtrai(4, 'CpfCnpj') <> '')
        then begin
         if Leitor.rCampo(tcStr, 'Cpf')<>''
          then NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
          else NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
        end;
      end;

     if (Leitor.rExtrai(4, 'Contato') <> '') or (Leitor.rExtrai(3, 'Contato') <> '')
      then begin
       NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
       NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
      end;

    end; // fim Tomador

  end; // fim InfDeclaracaoPrestacaoServico

 Result := True;
end;

function TNFSeR.LerRPS_IssDSF: Boolean;
var
 item: integer;
 ok  : Boolean;
 sOperacao, sTributacao: string;
begin
   VersaoXML := '1'; // para este provedor usar padrão "1".
   if (Leitor.rExtrai(1, 'Cabecalho') <> '') then begin
     	NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj        := Leitor.rCampo(tcStr, 'CPFCNPJRemetente');
     	NFSe.Prestador.Cnpj                                      := Leitor.rCampo(tcStr, 'CPFCNPJRemetente');
     	NFSe.PrestadorServico.RazaoSocial                        := Leitor.rCampo(tcStr, 'RazaoSocialRemetente');
     	NFSe.PrestadorServico.Endereco.CodigoMunicipio           := CodSiafiToCodCidade( Leitor.rCampo(tcStr, 'CodCidade') );
   end;

   if (Leitor.rExtrai(1, 'RPS') <> '') then begin

      NFSe.DataEmissaoRPS := Leitor.rCampo(tcDatHor, 'DataEmissaoRPS');
      NFSe.Status         := StrToEnumerado(ok, Leitor.rCampo(tcStr, 'Status'),['N','C'],[srNormal, srCancelado]);

      NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'NumeroRPS');
      NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'SerieRPS');
      NFSe.IdentificacaoRps.Tipo   := trRPS;//StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
      NFSe.InfID.ID                := OnlyNumber(NFSe.IdentificacaoRps.Numero);// + NFSe.IdentificacaoRps.Serie;
      NFSe.SeriePrestacao          := Leitor.rCampo(tcStr, 'SeriePrestacao');

     	NFSe.Prestador.InscricaoMunicipal     := Leitor.rCampo(tcStr, 'InscricaoMunicipalPrestador');
      NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'DDDPrestador') + Leitor.rCampo(tcStr, 'TelefonePrestador');

     	NFSe.Tomador.RazaoSocial              := Leitor.rCampo(tcStr, 'RazaoSocialTomador');
      NFSe.Tomador.Endereco.TipoLogradouro  := Leitor.rCampo(tcStr, 'TipoLogradouroTomador');
      NFSe.Tomador.Endereco.Endereco        := Leitor.rCampo(tcStr, 'LogradouroTomador');
      NFSe.Tomador.Endereco.Numero          := Leitor.rCampo(tcStr, 'NumeroEnderecoTomador');
      NFSe.Tomador.Endereco.Complemento     := Leitor.rCampo(tcStr, 'ComplementoEnderecoTomador');
      NFSe.Tomador.Endereco.TipoBairro      := Leitor.rCampo(tcStr, 'TipoBairroTomador');
      NFSe.Tomador.Endereco.Bairro          := Leitor.rCampo(tcStr, 'BairroTomador');
      NFSe.Tomador.Endereco.CEP             := Leitor.rCampo(tcStr, 'CEPTomador');
     	NFSe.Tomador.Endereco.CodigoMunicipio := CodSiafiToCodCidade( Leitor.rCampo(tcStr, 'CidadeTomador')) ;
      NFSe.Tomador.Endereco.xMunicipio      := Leitor.rCampo(tcStr, 'CidadeTomadorDescricao');
     	NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
     	NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipalTomador');
     	NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'CPFCNPJTomador');
      NFSe.Tomador.IdentificacaoTomador.DocTomadorEstrangeiro := Leitor.rCampo(tcStr, 'DocTomadorEstrangeiro');
      NFSe.Tomador.Contato.Email := Leitor.rCampo(tcStr, 'EmailTomador');
      NFSe.Tomador.Contato.Telefone          := Leitor.rCampo(tcStr, 'DDDTomador') + Leitor.rCampo(tcStr, 'TelefoneTomador');

      NFSe.Servico.CodigoCnae := Leitor.rCampo(tcStr, 'CodigoAtividade');
      NFSe.Servico.Valores.Aliquota := Leitor.rCampo(tcDe3, 'AliquotaAtividade');

      NFSe.Servico.Valores.IssRetido := StrToEnumerado( ok, Leitor.rCampo(tcStr, 'TipoRecolhimento'),
                                                        ['A','R'], [ stNormal, stRetencao{, stSubstituicao}]);

      NFSe.Servico.CodigoMunicipio := CodSiafiToCodCidade( Leitor.rCampo(tcStr, 'MunicipioPrestacao'));

      sOperacao   := AnsiUpperCase(Leitor.rCampo(tcStr, 'Operacao'));
      sTributacao := AnsiUpperCase(Leitor.rCampo(tcStr, 'Tributacao'));


      if sOperacao[1] in ['A', 'B'] then begin

         if (sOperacao = 'A') and (sTributacao = 'N') then
            NFSe.NaturezaOperacao := noNaoIncidencia
         else if sTributacao = 'G' then
            NFSe.NaturezaOperacao := noTributacaoForaMunicipio
         else if sTributacao = 'T' then
            NFSe.NaturezaOperacao := noTributacaoNoMunicipio;
      end
      else if (sOperacao = 'C') and (sTributacao = 'C') then begin
         NFSe.NaturezaOperacao := noIsencao;
      end
      else if (sOperacao = 'C') and (sTributacao = 'F') then begin
         NFSe.NaturezaOperacao := noImune;
      end;

      NFSe.NaturezaOperacao := StrToEnumerado( ok,sTributacao, ['T','K'], [ NFSe.NaturezaOperacao, noSuspensaDecisaoJudicial ]);

      NFSe.OptanteSimplesNacional := StrToEnumerado( ok,sTributacao, ['T','H'], [ snNao, snSim ]);

      NFSe.DeducaoMateriais := StrToEnumerado( ok,sOperacao, ['A','B'], [ snNao, snSim ]);

      NFse.RegimeEspecialTributacao := StrToEnumerado( ok,sTributacao, ['T','M'], [ retNenhum, retMicroempresarioIndividual ]);

      //NFSe.Servico.Valores.ValorDeducoes          :=
      NFSe.Servico.Valores.ValorPis               := Leitor.rCampo(tcDe2, 'ValorPIS');
      NFSe.Servico.Valores.ValorCofins            := Leitor.rCampo(tcDe2, 'ValorCOFINS');
      NFSe.Servico.Valores.ValorInss              := Leitor.rCampo(tcDe2, 'ValorINSS');
      NFSe.Servico.Valores.ValorIr                := Leitor.rCampo(tcDe2, 'ValorIR');
      NFSe.Servico.Valores.ValorCsll              := Leitor.rCampo(tcDe2, 'ValorCSLL');
      NFSe.Servico.Valores.AliquotaPIS            := Leitor.rCampo(tcDe2, 'AliquotaPIS');
      NFSe.Servico.Valores.AliquotaCOFINS         := Leitor.rCampo(tcDe2, 'AliquotaCOFINS');
      NFSe.Servico.Valores.AliquotaINSS           := Leitor.rCampo(tcDe2, 'AliquotaINSS');
      NFSe.Servico.Valores.AliquotaIR             := Leitor.rCampo(tcDe2, 'AliquotaIR');
      NFSe.Servico.Valores.AliquotaCSLL           := Leitor.rCampo(tcDe2, 'AliquotaCSLL');

      if (Leitor.rExtrai(1, 'Itens') <> '') then
      begin
        Item := 0 ;
        while (Leitor.rExtrai(1, 'Item', '', Item + 1) <> '') do
        begin
          FNfse.Servico.ItemServico.Add;
          FNfse.Servico.ItemServico[Item].Descricao     := Leitor.rCampo(tcStr, 'DiscriminacaoServico');
          FNfse.Servico.ItemServico[Item].Quantidade    := Leitor.rCampo(tcDe2, 'Quantidade');
          FNfse.Servico.ItemServico[Item].ValorUnitario := Leitor.rCampo(tcDe2, 'ValorUnitario');
          FNfse.Servico.ItemServico[Item].ValorTotal    := Leitor.rCampo(tcDe2, 'ValorTotal');
          FNfse.Servico.ItemServico[Item].Tributavel    := StrToEnumerado( ok,Leitor.rCampo(tcStr, 'Tributavel'), ['N','S'], [ snNao, snSim ]);
          FNfse.Servico.Valores.ValorServicos           := (FNfse.Servico.Valores.ValorServicos + FNfse.Servico.ItemServico[Item].ValorTotal);
          inc(Item);
        end;
      end;


      FNfse.Servico.Valores.ValorIss                          := (FNfse.Servico.Valores.ValorServicos * NFSe.Servico.Valores.Aliquota)/100;
      FNFSe.Servico.Valores.ValorLiquidoNfse                  := (FNfse.Servico.Valores.ValorServicos -
                                                                 (FNfse.Servico.Valores.ValorDeducoes +
                                                                  FNfse.Servico.Valores.DescontoCondicionado+
                                                                  FNfse.Servico.Valores.DescontoIncondicionado+
                                                                  FNFSe.Servico.Valores.ValorIssRetido));
      FNfse.Servico.Valores.BaseCalculo                       := NFSe.Servico.Valores.ValorLiquidoNfse;

      NFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'DescricaoRPS');
      NFSE.MotivoCancelamento := Leitor.rCampo(tcStr, 'MotCancelamento');
      NFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'CpfCnpjIntermediario');

  end; // fim Rps

 Result := True;
end;

function TNFSeR.LerRPS_Equiplano: Boolean;
var
  ok: Boolean;
  Item: Integer;
begin
  NFSe.IdentificacaoRps.Numero:= Leitor.rCampo(tcStr, 'nrRps');
  NFSe.IdentificacaoRps.Serie := Leitor.rCampo(tcStr, 'nrEmissorRps');
  NFSe.DataEmissao            := Leitor.rCampo(tcDatHor, 'dtEmissaoRps');
  NFSe.DataEmissaoRps         := Leitor.rCampo(tcDat, 'DataEmissao');
  NFSe.NaturezaOperacao       := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'NaturezaOperacao'));

  NFSe.Servico.Valores.IssRetido       := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'isIssRetido'));
  NFSe.Servico.Valores.ValorLiquidoNfse:= Leitor.rCampo(tcDe2, 'vlLiquidoRps');

  if (Leitor.rExtrai(2, 'tomador') <> '') then
    begin
      NFSe.Tomador.IdentificacaoTomador.CpfCnpj              := Leitor.rCampo(tcStr, 'nrDocumento');
      NFSe.Tomador.IdentificacaoTomador.DocTomadorEstrangeiro:= Leitor.rCampo(tcStr, 'dsDocumentoEstrangeiro');
      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual    := Leitor.rCampo(tcStr, 'nrInscricaoEstadual');
      NFSe.Tomador.RazaoSocial                               := Leitor.rCampo(tcStr, 'nmTomador');
      NFSe.Tomador.Contato.Email                             := Leitor.rCampo(tcStr, 'dsEmail');
      NFSe.Tomador.Endereco.Endereco                         := Leitor.rCampo(tcStr, 'dsEndereco');
      NFSe.Tomador.Endereco.Numero                           := Leitor.rCampo(tcStr, 'nrEndereco');
      NFSe.Tomador.Endereco.Complemento                      := Leitor.rCampo(tcStr, 'dsComplemento');
      NFSe.Tomador.Endereco.Bairro                           := Leitor.rCampo(tcStr, 'nmBairro');
      NFSe.Tomador.Endereco.CodigoMunicipio                  := Leitor.rCampo(tcStr, 'nrCidadeIbge');
      NFSe.Tomador.Endereco.UF                               := Leitor.rCampo(tcStr, 'nmUf');
      NFSe.Tomador.Endereco.xPais                            := Leitor.rCampo(tcStr, 'nmPais');
      NFSe.Tomador.Endereco.CEP                              := Leitor.rCampo(tcStr, 'nrCep');
      NFSe.Tomador.Contato.Telefone                          := Leitor.rCampo(tcStr, 'nrTelefone');
    end;

  if (Leitor.rExtrai(2, 'listaServicos') <> '') then
    begin
      NFSe.Servico.ItemListaServico            := Poem_Zeros( Leitor.rCampo(tcStr, 'nrServicoItem'), 2) +
                                                  Poem_Zeros( Leitor.rCampo(tcStr, 'nrServicoSubItem'), 2);

      Item := StrToIntDef(OnlyNumber(Nfse.Servico.ItemListaServico), 0);
      if Item<100 then Item:=Item*100+1;

      NFSe.Servico.ItemListaServico := FormatFloat('0000', Item);
      NFSe.Servico.ItemListaServico := Copy(NFSe.Servico.ItemListaServico, 1, 2) + '.' +
                                       Copy(NFSe.Servico.ItemListaServico, 3, 2);

      if TabServicosExt
       then NFSe.Servico.xItemListaServico := NotaUtil.ObterDescricaoServico(OnlyNumber(NFSe.Servico.ItemListaServico))
       else NFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(NFSe.Servico.ItemListaServico));

      NFSe.Servico.Valores.ValorServicos       := Leitor.rCampo(tcDe2, 'vlServico');
      NFSe.Servico.Valores.Aliquota            := Leitor.rCampo(tcDe2, 'vlAliquota');
      NFSe.Servico.Valores.ValorDeducoes       := Leitor.rCampo(tcDe2, 'vlDeducao');
      NFSe.Servico.Valores.JustificativaDeducao:= Leitor.rCampo(tcStr, 'dsJustificativaDeducao');
      NFSe.Servico.Valores.BaseCalculo         := Leitor.rCampo(tcDe2, 'vlBaseCalculo');
      NFSe.Servico.Valores.ValorIss            := Leitor.rCampo(tcDe2, 'vlIssServico');
      NFSe.Servico.Discriminacao               := Leitor.rCampo(tcStr, 'dsDiscriminacaoServico');
    end;

  if (Leitor.rExtrai(2, 'retencoes') <> '') then
    begin
      NFSe.Servico.Valores.ValorCofins            := Leitor.rCampo(tcDe2, 'vlCofins');
      NFSe.Servico.Valores.ValorCsll              := Leitor.rCampo(tcDe2, 'vlCsll');
      NFSe.Servico.Valores.ValorInss              := Leitor.rCampo(tcDe2, 'vlInss');
      NFSe.Servico.Valores.ValorIr                := Leitor.rCampo(tcDe2, 'vlIrrf');
      NFSe.Servico.Valores.ValorPis               := Leitor.rCampo(tcDe2, 'vlPis');
      NFSe.Servico.Valores.ValorIssRetido         := Leitor.rCampo(tcDe2, 'vlIss');
      NFSe.Servico.Valores.AliquotaCofins         := Leitor.rCampo(tcDe2, 'vlAliquotaCofins');
      NFSe.Servico.Valores.AliquotaCsll           := Leitor.rCampo(tcDe2, 'vlAliquotaCsll');
      NFSe.Servico.Valores.AliquotaInss           := Leitor.rCampo(tcDe2, 'vlAliquotaInss');
      NFSe.Servico.Valores.AliquotaIr             := Leitor.rCampo(tcDe2, 'vlAliquotaIrrf');
      NFSe.Servico.Valores.AliquotaPis            := Leitor.rCampo(tcDe2, 'vlAliquotaPis');
    end;

  Result := True;
end;

function TNFSeR.LerRPS: Boolean;
var
 CM: String;
 Ok: Boolean;
begin

 Result := False;

 if FProvedor = proNenhum then
 begin
   if (Leitor.rExtrai(1, 'OrgaoGerador') <> '') then
   begin
     CM := Leitor.rCampo(tcStr, 'CodigoMunicipio');
     FProvedor := StrToProvedor(Ok, CodCidadeToProvedor(StrToIntDef(CM, 0)));
   end;

   if (CM = '') or (CM = '0') then
   begin
     if (Leitor.rExtrai(1, 'Servico') <> '') then
     begin
       CM := Leitor.rCampo(tcStr, 'CodigoMunicipio');
       FProvedor := StrToProvedor(Ok, CodCidadeToProvedor(StrToIntDef(CM, 0)));
     end;
   end;

    { Alterado Por Cleiver em - 22-08-2014 } // ISSDSF
   if (Leitor.rExtrai(1, 'Cabecalho') <> '') then
   begin
     CM := CodSiafiToCodCidade( Leitor.rCampo(tcStr, 'CodCidade') );
     FProvedor := StrToProvedor(Ok, CodCidadeToProvedor(StrToIntDef(CM, 0)));
   end;

   if (CM = '') or (CM = '0') then
   begin
     if (Leitor.rExtrai(1, 'PrestadorServico') <> '') then
     begin
       CM := OnlyNumber(Leitor.rCampo(tcStr, 'CodigoMunicipio'));
       if CM = '' then
         CM := Leitor.rCampo(tcStr, 'Cidade');
       FProvedor := StrToProvedor(Ok, CodCidadeToProvedor(StrToIntDef(CM, 0)));
     end
     else
       FProvedor := proNenhum;
   end;
   { Alterado Por Cleiver em - 22-08-2014 }
   if (FProvedor = proNenhum) and (Pos('https://nfse.goiania.go.gov.br/ws/', Leitor.Arquivo) > 0)  then
     FProvedor := proGoiania;
 end;

 { Alterado Por Cleiver em - 22-08-2014 }
 if (Leitor.rExtrai(1, 'Rps') <> '') or (Leitor.rExtrai(1, 'RPS') <> '') then
 begin

   case FProvedor of
    proABRASFv1,
    proAbaco,
    proBetha,
    proBHISS,
    proDBSeller,
    proFISSLex,
    proGinfes,
    proGovBR,
    proISSCuritiba,
    proISSIntel,
    proISSNet,
    proLexsom,
    proNatal,
    proNFSeBrasil,
    proProdemge,
    proPronim,
    proPublica,
    proRecife,
    proRJ,
    proSimplISS,
    proSJP,
    proSpeedGov,
    proThema,
    proTinus,
    proTiplan,
    proWebISS: Result := LerRPS_ABRASF_V1;

    proABRASFv2,
    pro4R,
    proActcon,
    proAgili,
    proCoplan,
    proDigifred,
    proFIntelISS,
    proFiorilli,
    proFreire,
    proGoiania,
    proGovDigital,
    proISSDigital,
    proISSe,
    proLink3,
    proMitra,
    proProdata,
    proPVH,
    proSaatri,
    proSisPMJP,
    proSystemPro,
    proTecnos,
    ProVirtual,
    proVitoria: Result := LerRPS_ABRASF_V2;

    proIssDsf:  Result := LerRPS_IssDsf;

    proEquiplano: Result := LerRPS_Equiplano;
   end;
 end;

end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

function TNFSeR.LerNFSe_ABRASF_V1: Boolean;
var
 item, I: Integer;
 ok  : Boolean;
begin
  if (Leitor.rExtrai(3, 'IdentificacaoRps') <> '')
   then begin
    NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
    NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
    NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
    NFSe.InfID.ID                := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
   end;

  if (Leitor.rExtrai(3, 'Servico') <> '')
   then begin
    NFSe.Servico.ItemListaServico := OnlyNumber(Leitor.rCampo(tcStr, 'ItemListaServico'));

    Item := StrToIntDef(OnlyNumber(Nfse.Servico.ItemListaServico), 0);
    if Item<100 then Item:=Item*100+1;

    NFSe.Servico.ItemListaServico := FormatFloat('0000', Item);
    NFSe.Servico.ItemListaServico := Copy(NFSe.Servico.ItemListaServico, 1, 2) + '.' +
                                     Copy(NFSe.Servico.ItemListaServico, 3, 2);

    if TabServicosExt
     then NFSe.Servico.xItemListaServico := NotaUtil.ObterDescricaoServico(OnlyNumber(NFSe.Servico.ItemListaServico))
     else NFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(NFSe.Servico.ItemListaServico));

    NFSe.Servico.CodigoCnae                := Leitor.rCampo(tcStr, 'CodigoCnae');
    NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio');
    NFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'Discriminacao');
    NFSe.Servico.Descricao                 := '';
    NFSe.Servico.CodigoMunicipio           := Leitor.rCampo(tcStr, 'CodigoMunicipio');

//    NFSe.Servico.ResponsavelRetencao       := StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'ResponsavelRetencao'));
//    NFSe.Servico.CodigoPais          := Leitor.rCampo(tcInt, 'CodigoPais');
//    NFSe.Servico.ExigibilidadeISS    := StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'ExigibilidadeISS'));
//    NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'MunicipioIncidencia');
//    if NFSe.Servico.MunicipioIncidencia =0
//     then NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'CodigoMunicipio');

    if (Leitor.rExtrai(4, 'Valores') <> '')
     then begin
      NFSe.Servico.Valores.ValorServicos          := Leitor.rCampo(tcDe2, 'ValorServicos');
      NFSe.Servico.Valores.ValorDeducoes          := Leitor.rCampo(tcDe2, 'ValorDeducoes');
      NFSe.Servico.Valores.ValorPis               := Leitor.rCampo(tcDe2, 'ValorPis');
      NFSe.Servico.Valores.ValorCofins            := Leitor.rCampo(tcDe2, 'ValorCofins');
      NFSe.Servico.Valores.ValorInss              := Leitor.rCampo(tcDe2, 'ValorInss');
      NFSe.Servico.Valores.ValorIr                := Leitor.rCampo(tcDe2, 'ValorIr');
      NFSe.Servico.Valores.ValorCsll              := Leitor.rCampo(tcDe2, 'ValorCsll');
      NFSe.Servico.Valores.IssRetido              := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
      NFSe.Servico.Valores.ValorIss               := Leitor.rCampo(tcDe2, 'ValorIss');
      NFSe.Servico.Valores.OutrasRetencoes        := Leitor.rCampo(tcDe2, 'OutrasRetencoes');
      NFSe.Servico.Valores.BaseCalculo            := Leitor.rCampo(tcDe2, 'BaseCalculo');
      NFSe.Servico.Valores.Aliquota               := Leitor.rCampo(tcDe3, 'Aliquota');
      NFSe.Servico.Valores.ValorLiquidoNfse       := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');
      NFSe.Servico.Valores.ValorIssRetido         := Leitor.rCampo(tcDe2, 'ValorIssRetido');
      NFSe.Servico.Valores.DescontoCondicionado   := Leitor.rCampo(tcDe2, 'DescontoCondicionado');
      NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'DescontoIncondicionado');
     end;

    // Incluido por Italo em 29/09/2014
    if NFSe.Servico.Valores.ValorLiquidoNfse = 0 then
      NFSe.Servico.Valores.ValorLiquidoNfse := NFSe.Servico.Valores.ValorServicos -
                                               NFSe.Servico.Valores.DescontoIncondicionado -
                                               NFSe.Servico.Valores.DescontoCondicionado -
                                               // Retenções Federais
                                               NFSe.Servico.Valores.ValorPis -
                                               NFSe.Servico.Valores.ValorCofins -
                                               NFSe.Servico.Valores.ValorIr -
                                               NFSe.Servico.Valores.ValorInss -
                                               NFSe.Servico.Valores.ValorCsll -

                                               NFSe.Servico.Valores.OutrasRetencoes -
                                               NFSe.Servico.Valores.ValorIssRetido;

    if NFSe.Servico.Valores.BaseCalculo = 0 then
      NFSe.Servico.Valores.BaseCalculo := NFSe.Servico.Valores.ValorServicos -
                                          NFSe.Servico.Valores.ValorDeducoes -
                                          NFSe.Servico.Valores.DescontoIncondicionado;

    if NFSe.Servico.Valores.ValorIss = 0 then
      NFSe.Servico.Valores.ValorIss := (NFSe.Servico.Valores.BaseCalculo * NFSe.Servico.Valores.Aliquota)/100;
   end; // fim serviço

  if Leitor.rExtrai(3, 'PrestadorServico') <> ''
   then begin
    NFSe.PrestadorServico.RazaoSocial  := Leitor.rCampo(tcStr, 'RazaoSocial');
    NFSe.PrestadorServico.NomeFantasia := Leitor.rCampo(tcStr, 'NomeFantasia');

    NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
    if NFSe.PrestadorServico.Endereco.Endereco = '' then
    begin
      NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
      if Copy(NFSe.PrestadorServico.Endereco.Endereco, 1, 10) = '<Endereco>'
       then NFSe.PrestadorServico.Endereco.Endereco := Copy(NFSe.PrestadorServico.Endereco.Endereco, 11, 125);
    end;

    NFSe.PrestadorServico.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
    NFSe.PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
    NFSe.PrestadorServico.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

    if VersaoXML = '1'
     then begin
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
      NFSe.PrestadorServico.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
     end
     else begin
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
      NFSe.PrestadorServico.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
     end;

//    NFSe.PrestadorServico.Endereco.CodigoPais := Leitor.rCampo(tcInt, 'CodigoPais');
    NFSe.PrestadorServico.Endereco.CEP        := Leitor.rCampo(tcStr, 'Cep');

    if length(NFSe.PrestadorServico.Endereco.CodigoMunicipio)<7
     then NFSe.PrestadorServico.Endereco.CodigoMunicipio := Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 1, 2) +
          FormatFloat('00000', StrToIntDef(Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 3, 5), 0));

    NFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));

    if (Leitor.rExtrai(4, 'IdentificacaoPrestador') <> '')
     then begin
      NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

      if VersaoXML = '1'
       then begin
        if Leitor.rExtrai(5, 'CpfCnpj') <> ''
         then begin
          NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cpf');
           if NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = ''
            then NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
         end;
       end
       else begin
        NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
       end;
      end;

    if Leitor.rExtrai(4, 'Contato') <> ''
     then begin
      NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
      NFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
     end;

   end; // fim PrestadorServico

  if Leitor.rExtrai(3, 'TomadorServico') <> ''
   then begin
    NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');

    NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
    if NFSe.Tomador.Endereco.Endereco = '' then
    begin
      NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
      if Copy(NFSe.Tomador.Endereco.Endereco, 1, 10) = '<Endereco>'
       then NFSe.Tomador.Endereco.Endereco := Copy(NFSe.Tomador.Endereco.Endereco, 11, 125);
    end;

    NFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
    NFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
    NFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

    if VersaoXML = '1'
     then begin
      NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
      NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
     end
     else begin
      NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
      NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
     end;

    NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

    if length(NFSe.Tomador.Endereco.CodigoMunicipio)<7
      then NFSe.Tomador.Endereco.CodigoMunicipio := Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
           FormatFloat('00000', StrToIntDef(Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

    if NFSe.Tomador.Endereco.UF = ''
     then NFSe.Tomador.Endereco.UF := NFSe.PrestadorServico.Endereco.UF;

     NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));

    if Leitor.rExtrai(4, 'IdentificacaoTomador') <> ''
     then begin
      NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
      if Leitor.rExtrai(5, 'CpfCnpj') <> ''
       then begin
        if Leitor.rCampo(tcStr, 'Cpf')<>''
         then NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
         else NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
       end;
      end;

    if Leitor.rExtrai(4, 'Contato') <> ''
     then begin
      NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
      NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
     end;
   end;

  if Leitor.rExtrai(3, 'IntermediarioServico') <> ''
   then begin
    NFSe.IntermediarioServico.RazaoSocial        := Leitor.rCampo(tcStr, 'RazaoSocial');
    NFSe.IntermediarioServico.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
    if Leitor.rExtrai(4, 'CpfCnpj') <> ''
     then begin
      if Leitor.rCampo(tcStr, 'Cpf')<>''
       then NFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
       else NFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
     end;
   end;

  if Leitor.rExtrai(3, 'OrgaoGerador') <> ''
   then begin
    NFSe.OrgaoGerador.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
    NFSe.OrgaoGerador.Uf              := Leitor.rCampo(tcStr, 'Uf');
   end; // fim OrgaoGerador

  if Leitor.rExtrai(3, 'ConstrucaoCivil') <> ''
   then begin
    NFSe.ConstrucaoCivil.CodigoObra := Leitor.rCampo(tcStr, 'CodigoObra');
    NFSe.ConstrucaoCivil.Art        := Leitor.rCampo(tcStr, 'Art');
   end;

   // adicionado por Tailan Bonassi
   if FProvedor in [proBetha] then
     if Leitor.rExtrai(3, 'CondicaoPagamento') <> ''
     then begin
      NFSe.CondicaoPagamento.Condicao:= StrToCondicao(ok,Leitor.rCampo(tcStr,'Condicao'));
      NFSe.CondicaoPagamento.QtdParcela:= Leitor.rCampo(tcInt,'Condicao');
      for I := 0 to 9999
      do begin
       if (Leitor.rExtrai(4, 'Parcelas', 'Parcelas', i) <> '')
        then begin
         with NFSe.CondicaoPagamento.Parcelas.Add
          do begin
           Parcela        := Leitor.rCampo(tcInt, 'Parcela');
           DataVencimento := Leitor.rCampo(tcDatVcto, 'DataVencimento');
           Valor          := Leitor.rCampo(tcDe2, 'Valor');
         end;
        end else Break;
      end;
     end;

 Result := True;
end;

function TNFSeR.LerNFSe_ABRASF_V2: Boolean;
var
 item: Integer;
 ok  : Boolean;
begin
  if Leitor.rExtrai(3, 'ValoresNfse') <> '' then
  begin
    NFSe.ValoresNfse.BaseCalculo      := Leitor.rCampo(tcDe2, 'BaseCalculo');
    NFSe.ValoresNfse.Aliquota         := Leitor.rCampo(tcDe3, 'Aliquota');
    NFSe.ValoresNfse.ValorIss         := Leitor.rCampo(tcDe2, 'ValorIss');
    NFSe.ValoresNfse.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');

    if (FProvedor = proCoplan) then
    begin
      NFSe.Servico.Valores.BaseCalculo      := Leitor.rCampo(tcDe2, 'BaseCalculo');
      NFSe.Servico.Valores.Aliquota         := Leitor.rCampo(tcDe3, 'Aliquota');
      NFSe.Servico.Valores.ValorIss         := Leitor.rCampo(tcDe2, 'ValorIss');
      NFSe.Servico.Valores.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');
    end;
  end; // fim ValoresNfse

  if Leitor.rExtrai(3, 'PrestadorServico') <> ''
   then begin
    NFSe.PrestadorServico.RazaoSocial  := Leitor.rCampo(tcStr, 'RazaoSocial');
    NFSe.PrestadorServico.NomeFantasia := Leitor.rCampo(tcStr, 'NomeFantasia');

    NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
    if NFSe.PrestadorServico.Endereco.Endereco = '' then
    begin
      NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
      if Copy(NFSe.PrestadorServico.Endereco.Endereco, 1, 10) = '<Endereco>'
       then NFSe.PrestadorServico.Endereco.Endereco := Copy(NFSe.PrestadorServico.Endereco.Endereco, 11, 125);
    end;

    NFSe.PrestadorServico.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
    NFSe.PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
    NFSe.PrestadorServico.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

    if VersaoXML = '1'
     then begin
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
      NFSe.PrestadorServico.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
     end
     else begin
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
      NFSe.PrestadorServico.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
     end;

    NFSe.PrestadorServico.Endereco.CodigoPais := Leitor.rCampo(tcInt, 'CodigoPais');
    NFSe.PrestadorServico.Endereco.CEP        := Leitor.rCampo(tcStr, 'Cep');

    if length(NFSe.PrestadorServico.Endereco.CodigoMunicipio)<7
     then NFSe.PrestadorServico.Endereco.CodigoMunicipio := Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 1, 2) +
          FormatFloat('00000', StrToIntDef(Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 3, 5), 0));

    NFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));

    if (Leitor.rExtrai(4, 'IdentificacaoPrestador') <> '')
     then begin
      NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

      if (VersaoXML = '1') or (FProvedor = proFiorilli)
       then begin
        if Leitor.rExtrai(5, 'CpfCnpj') <> ''
         then begin
          NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cpf');
           if NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = ''
            then NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
         end;
       end
       else begin
        NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
       end;
      end;

    if Leitor.rExtrai(4, 'Contato') <> ''
     then begin
      NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
      NFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
     end;

   end; // fim PrestadorServico

 if Leitor.rExtrai(3, 'EnderecoPrestadorServico') <> ''
  then begin
   NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
   if NFSe.PrestadorServico.Endereco.Endereco = '' then
   begin
     NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
     if Copy(NFSe.PrestadorServico.Endereco.Endereco, 1, 10) = '<Endereco>'
      then NFSe.PrestadorServico.Endereco.Endereco := Copy(NFSe.PrestadorServico.Endereco.Endereco, 11, 125);
   end;

   NFSe.PrestadorServico.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
   NFSe.PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
   NFSe.PrestadorServico.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

   if VersaoXML = '1'
    then begin
     NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
     NFSe.PrestadorServico.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
    end
    else begin
     NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
     NFSe.PrestadorServico.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
    end;

   NFSe.PrestadorServico.Endereco.CodigoPais := Leitor.rCampo(tcInt, 'CodigoPais');
   NFSe.PrestadorServico.Endereco.CEP        := Leitor.rCampo(tcStr, 'Cep');

   if length(NFSe.PrestadorServico.Endereco.CodigoMunicipio)<7
    then NFSe.PrestadorServico.Endereco.CodigoMunicipio := Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 1, 2) +
           FormatFloat('00000', StrToIntDef(Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 3, 5), 0));

   NFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));

  end; // fim EnderecoPrestadorServico

 if Leitor.rExtrai(3, 'OrgaoGerador') <> ''
  then begin
   NFSe.OrgaoGerador.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
   NFSe.OrgaoGerador.Uf              := Leitor.rCampo(tcStr, 'Uf');
  end; // fim OrgaoGerador

 if (Leitor.rExtrai(3, 'InfDeclaracaoPrestacaoServico') <> '') or (Leitor.rExtrai(3, 'DeclaracaoPrestacaoServico') <> '')
  then begin
   NFSe.Competencia              := Leitor.rCampo(tcStr, 'Competencia');
   NFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
   NFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
   NFSe.IncentivadorCultural     := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivoFiscal'));

   if (Leitor.rExtrai(4, 'Rps') <> '')
    then begin
     NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissao');
     NFSe.Status         := StrToStatusRPS(ok, Leitor.rCampo(tcStr, 'Status'));

     if (Leitor.rExtrai(5, 'IdentificacaoRps') <> '')
      then begin
       NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
       NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
       NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
       NFSe.InfID.ID                := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
      end;

     if (Leitor.rExtrai(5, 'RpsSubstituido') <> '')
      then begin
       NFSe.RpsSubstituido.Numero := Leitor.rCampo(tcStr, 'Numero');
       NFSe.RpsSubstituido.Serie  := Leitor.rCampo(tcStr, 'Serie');
       NFSe.RpsSubstituido.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
      end;
    end
    else begin
     NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissao');
     NFSe.Status         := StrToStatusRPS(ok, Leitor.rCampo(tcStr, 'Status'));

     if (Leitor.rExtrai(4, 'IdentificacaoRps') <> '')
      then begin
       NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
       NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
       NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
       NFSe.InfID.ID                := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
      end;

     if (Leitor.rExtrai(4, 'RpsSubstituido') <> '')
      then begin
       NFSe.RpsSubstituido.Numero := Leitor.rCampo(tcStr, 'Numero');
       NFSe.RpsSubstituido.Serie  := Leitor.rCampo(tcStr, 'Serie');
       NFSe.RpsSubstituido.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
      end;
    end;

   if (Leitor.rExtrai(4, 'Servico') <> '')
    then begin
     NFSe.Servico.Valores.IssRetido   := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
     NFSe.Servico.ResponsavelRetencao := StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'ResponsavelRetencao'));
     NFSe.Servico.ItemListaServico    := OnlyNumber(Leitor.rCampo(tcStr, 'ItemListaServico'));

     Item := StrToIntDef(OnlyNumber(Nfse.Servico.ItemListaServico), 0);
     if Item<100 then Item:=Item*100+1;

     NFSe.Servico.ItemListaServico := FormatFloat('0000', Item);
     NFSe.Servico.ItemListaServico := Copy(NFSe.Servico.ItemListaServico, 1, 2) + '.' +
                                      Copy(NFSe.Servico.ItemListaServico, 3, 2);

     if TabServicosExt
      then NFSe.Servico.xItemListaServico := NotaUtil.ObterDescricaoServico(OnlyNumber(NFSe.Servico.ItemListaServico))
      else NFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(NFSe.Servico.ItemListaServico));

     NFSe.Servico.CodigoCnae                := Leitor.rCampo(tcStr, 'CodigoCnae');
     NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio');
     NFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'Discriminacao');
     NFSe.Servico.Descricao                 := '';
     NFSe.Servico.CodigoMunicipio           := Leitor.rCampo(tcStr, 'CodigoMunicipio');
     NFSe.Servico.CodigoPais                := Leitor.rCampo(tcInt, 'CodigoPais');
     NFSe.Servico.ExigibilidadeISS          := StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'ExigibilidadeISS'));
     NFSe.Servico.MunicipioIncidencia       := Leitor.rCampo(tcInt, 'MunicipioIncidencia');
     if NFSe.Servico.MunicipioIncidencia = 0
      then NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'CodigoMunicipio');

     if (Leitor.rExtrai(5, 'Valores') <> '')
      then begin
        NFSe.Servico.Valores.ValorServicos   := Leitor.rCampo(tcDe2, 'ValorServicos');
        NFSe.Servico.Valores.ValorDeducoes   := Leitor.rCampo(tcDe2, 'ValorDeducoes');
        NFSe.Servico.Valores.ValorPis        := Leitor.rCampo(tcDe2, 'ValorPis');
        NFSe.Servico.Valores.ValorCofins     := Leitor.rCampo(tcDe2, 'ValorCofins');
        NFSe.Servico.Valores.ValorInss       := Leitor.rCampo(tcDe2, 'ValorInss');
        NFSe.Servico.Valores.ValorIr         := Leitor.rCampo(tcDe2, 'ValorIr');
        NFSe.Servico.Valores.ValorCsll       := Leitor.rCampo(tcDe2, 'ValorCsll');
        NFSe.Servico.Valores.OutrasRetencoes := Leitor.rCampo(tcDe2, 'OutrasRetencoes');
        NFSe.Servico.Valores.ValorIss        := Leitor.rCampo(tcDe2, 'ValorIss');
//        NFSe.Servico.Valores.BaseCalculo            := Leitor.rCampo(tcDe2, 'BaseCalculo');
        NFSe.Servico.Valores.Aliquota        := Leitor.rCampo(tcDe3, 'Aliquota');

        if (FProvedor = proISSe) then // Joel Takei 10/12/2014
        begin
          if NFSe.Servico.Valores.IssRetido = stRetencao then
            NFSe.Servico.Valores.ValorIssRetido := Leitor.rCampo(tcDe2, 'ValorIss')
          else
            NFSe.Servico.Valores.ValorIssRetido := 0;
        end
        else
          NFSe.Servico.Valores.ValorIssRetido := Leitor.rCampo(tcDe2, 'ValorIssRetido');

//        NFSe.Servico.Valores.ValorIssRetido         := Leitor.rCampo(tcDe2, 'ValorIssRetido');
        NFSe.Servico.Valores.DescontoCondicionado   := Leitor.rCampo(tcDe2, 'DescontoCondicionado');
        NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'DescontoIncondicionado');

        { Alterado Por Cleiver em - 22-08-2014 }
        // Alterado por Italo em 10/09/2014
        if NFSe.Servico.Valores.ValorLiquidoNfse = 0 then
          NFSe.Servico.Valores.ValorLiquidoNfse     := NFSe.Servico.Valores.ValorServicos -
                                                       NFSe.Servico.Valores.DescontoIncondicionado -
                                                       NFSe.Servico.Valores.DescontoCondicionado -
                                                       // Retenções Federais
                                                       NFSe.Servico.Valores.ValorPis -
                                                       NFSe.Servico.Valores.ValorCofins -
                                                       NFSe.Servico.Valores.ValorIr -
                                                       NFSe.Servico.Valores.ValorInss -
                                                       NFSe.Servico.Valores.ValorCsll -

                                                       NFSe.Servico.Valores.OutrasRetencoes -
                                                       NFSe.Servico.Valores.ValorIssRetido;

        if NFSe.Servico.Valores.BaseCalculo = 0 then
          NFSe.Servico.Valores.BaseCalculo          := NFSe.Servico.Valores.ValorServicos -
                                                       NFSe.Servico.Valores.ValorDeducoes -
                                                       NFSe.Servico.Valores.DescontoIncondicionado;

        if NFSe.Servico.Valores.ValorIss = 0 then
          NFSe.Servico.Valores.ValorIss             := (NFSe.Servico.Valores.BaseCalculo * NFSe.Servico.Valores.Aliquota)/100;

      end;
    end; // fim serviço

   if (Leitor.rExtrai(4, 'Prestador') <> '')
    then begin
     NFSe.Prestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

     if (VersaoXML = '1') or (FProvedor in [proFiorilli, proGoiania, ProTecnos, proVirtual, proDigifred])
      then begin
       if (FProvedor = proTecnos)  then
       begin
         NFSe.PrestadorServico.RazaoSocial  := Leitor.rCampo(tcStr, 'RazaoSocial');
         NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
       end;
       if Leitor.rExtrai(5, 'CpfCnpj') <> ''
        then begin
          NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cpf');
          if NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = ''
           then NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
        end;
       NFSe.Prestador.Cnpj := NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
      end
      else begin
       NFSe.Prestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
      end;
    end; // fim Prestador

   if (Leitor.rExtrai(4, 'TomadorServico') <> '') or (Leitor.rExtrai(4, 'Tomador') <> '')
    then begin
     NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');

     NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
     if NFSe.Tomador.Endereco.Endereco = '' then
     begin
       NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
       if Copy(NFSe.Tomador.Endereco.Endereco, 1, 10) = '<Endereco>'
        then NFSe.Tomador.Endereco.Endereco := Copy(NFSe.Tomador.Endereco.Endereco, 11, 125);
     end;

     NFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
     NFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
     NFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

     if VersaoXML = '1'
      then begin
       NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
       NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
      end
      else begin
       NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
       NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
      end;

     NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

     if length(NFSe.Tomador.Endereco.CodigoMunicipio) < 7
      then NFSe.Tomador.Endereco.CodigoMunicipio := Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
        FormatFloat('00000', StrToIntDef(Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

     if NFSe.Tomador.Endereco.UF = ''
      then NFSe.Tomador.Endereco.UF := NFSe.PrestadorServico.Endereco.UF;

     NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));

     if (Leitor.rExtrai(5, 'IdentificacaoTomador') <> '')
      then begin
       NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

       if Leitor.rExtrai(6, 'CpfCnpj') <> ''
        then begin
         if Leitor.rCampo(tcStr, 'Cpf')<>''
          then NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
          else NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
        end;
      end;

     if (Leitor.rExtrai(5, 'Contato') <> '')
      then begin
       NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
       NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
      end;
    end; // fim Tomador
  end; // fim InfDeclaracaoPrestacaoServico

 Result := True;
end;

function TNFSeR.LerNFSe_IssDSF: Boolean;
var
  ok: Boolean;
  Item{, posI, count}: integer;
  sOperacao, sTributacao: string;
//    strItem: ansiString;
//    leitorItem : TLeitor;
begin
 Leitor.Grupo := Leitor.Arquivo;
 //provedorIssDSF
 if (Pos('<Notas>', Leitor.Arquivo) > 0) or (Pos('<Nota>', Leitor.Arquivo) > 0)
  then begin
   VersaoXML := '1'; // para este provedor usar padrão "1".

   FNFSe.Numero            := Leitor.rCampo(tcStr, 'NumeroNota');
   FNFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');

   FNFSe.DataEmissaoRps := Leitor.rCampo(tcDatHor, 'DataEmissaoRPS');
   FNFSe.Competencia    := Copy(Leitor.rCampo(tcDat, 'DataEmissaoRPS'),7,4) + Copy(Leitor.rCampo(tcDat, 'DataEmissaoRPS'),4,2);
   FNFSe.DataEmissao    := Leitor.rCampo(tcDatHor, 'DataProcessamento');
   FNFSe.Status         := StrToEnumerado(ok, Leitor.rCampo(tcStr, 'SituacaoRPS'),['N','C'],[srNormal, srCancelado]);

   NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'NumeroRPS');
   NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'SerieRPS');
   NFSe.IdentificacaoRps.Tipo   := trRPS; //StrToTipoRPS(ok, leitorAux.rCampo(tcStr, 'Tipo'));
   NFSe.InfID.ID                := OnlyNumber(NFSe.IdentificacaoRps.Numero);// + NFSe.IdentificacaoRps.Serie;
   NFSe.SeriePrestacao          := Leitor.rCampo(tcStr, 'SeriePrestacao');

   NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipalTomador');
   NFSe.Tomador.IdentificacaoTomador.CpfCnpj            := Leitor.rCampo(tcStr, 'CPFCNPJTomador');
   NFSe.Tomador.RazaoSocial                             := Leitor.rCampo(tcStr, 'RazaoSocialTomador');
   NFSe.Tomador.Endereco.TipoLogradouro                 := Leitor.rCampo(tcStr, 'TipoLogradouroTomador');
   NFSe.Tomador.Endereco.Endereco                       := Leitor.rCampo(tcStr, 'LogradouroTomador');
   NFSe.Tomador.Endereco.Numero                         := Leitor.rCampo(tcStr, 'NumeroEnderecoTomador');
   NFSe.Tomador.Endereco.Complemento                    := Leitor.rCampo(tcStr, 'ComplementoEnderecoTomador');
   NFSe.Tomador.Endereco.TipoBairro                     := Leitor.rCampo(tcStr, 'TipoBairroTomador');
   NFSe.Tomador.Endereco.Bairro                         := Leitor.rCampo(tcStr, 'BairroTomador');
   NFSe.Tomador.Endereco.CodigoMunicipio                := CodSiafiToCodCidade( Leitor.rCampo(tcStr, 'CidadeTomador')) ;
   NFSe.Tomador.Endereco.xMunicipio                     := CodCidadeToCidade( StrToInt(NFSe.Tomador.Endereco.CodigoMunicipio) ) ;
   NFSe.Tomador.Endereco.UF                             := CodigoParaUF( StrToInt(Copy(NFSe.Tomador.Endereco.CodigoMunicipio,1,2) ));
   NFSe.Tomador.Endereco.CEP                            := Leitor.rCampo(tcStr, 'CEPTomador');
   NFSe.Tomador.Contato.Email                           := Leitor.rCampo(tcStr, 'EmailTomador');

   NFSe.Servico.CodigoCnae                              := Leitor.rCampo(tcStr, 'CodigoAtividade');
   NFSe.Servico.Valores.Aliquota                        := Leitor.rCampo(tcDe3, 'AliquotaAtividade');

   NFSe.Servico.Valores.IssRetido                       := StrToEnumerado( ok, Leitor.rCampo(tcStr, 'TipoRecolhimento'),
                                                           ['A','R'], [ stNormal, stRetencao{, stSubstituicao}]);

   NFSe.Servico.CodigoMunicipio                         := CodSiafiToCodCidade( Leitor.rCampo(tcStr, 'MunicipioPrestacao'));

   sOperacao                                            := AnsiUpperCase(Leitor.rCampo(tcStr, 'Operacao'));
   sTributacao                                          := AnsiUpperCase(Leitor.rCampo(tcStr, 'Tributacao'));

   if sOperacao[1] in ['A', 'B'] then begin
      if NFSe.Servico.CodigoMunicipio = NFSe.PrestadorServico.Endereco.CodigoMunicipio then
         NFSe.NaturezaOperacao := noTributacaoNoMunicipio      // ainda estamos
      else                                                    // em análise sobre
         NFSe.NaturezaOperacao := noTributacaoForaMunicipio;   // este ponto
   end
   else if (sOperacao = 'C') and (sTributacao = 'C') then begin
      NFSe.NaturezaOperacao := noIsencao;
   end
   else if (sOperacao = 'C') and (sTributacao = 'F') then begin
      NFSe.NaturezaOperacao := noImune;
   end
   else if (sOperacao = 'A') and (sTributacao = 'N') then begin
      NFSe.NaturezaOperacao := noNaoIncidencia;
   end;

   NFSe.NaturezaOperacao := StrToEnumerado( ok,sTributacao, ['T','K'], [ NFSe.NaturezaOperacao, noSuspensaDecisaoJudicial ]);

   NFSe.OptanteSimplesNacional := StrToEnumerado( ok,sTributacao, ['T','H'], [ snNao, snSim ]);

   NFSe.DeducaoMateriais := StrToEnumerado( ok,sOperacao, ['A','B'], [ snNao, snSim ]);

   NFse.RegimeEspecialTributacao := StrToEnumerado( ok,sTributacao, ['T','M'], [ retNenhum, retMicroempresarioIndividual ]);

   NFSe.Servico.Valores.ValorPis        := Leitor.rCampo(tcDe2, 'ValorPIS');
   NFSe.Servico.Valores.ValorCofins     := Leitor.rCampo(tcDe2, 'ValorCOFINS');
   NFSe.Servico.Valores.ValorInss       := Leitor.rCampo(tcDe2, 'ValorINSS');
   NFSe.Servico.Valores.ValorIr         := Leitor.rCampo(tcDe2, 'ValorIR');
   NFSe.Servico.Valores.ValorCsll       := Leitor.rCampo(tcDe2, 'ValorCSLL');
   NFSe.Servico.Valores.AliquotaPIS     := Leitor.rCampo(tcDe2, 'AliquotaPIS');
   NFSe.Servico.Valores.AliquotaCOFINS  := Leitor.rCampo(tcDe2, 'AliquotaCOFINS');
   NFSe.Servico.Valores.AliquotaINSS    := Leitor.rCampo(tcDe2, 'AliquotaINSS');
   NFSe.Servico.Valores.AliquotaIR      := Leitor.rCampo(tcDe2, 'AliquotaIR');
   NFSe.Servico.Valores.AliquotaCSLL    := Leitor.rCampo(tcDe2, 'AliquotaCSLL');

   NFSe.OutrasInformacoes                 := '';//Leitor.rCampo(tcStr, 'DescricaoRPS');
   NFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'DescricaoRPS');
   NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoAtividade');

   NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'DDDPrestador') + Leitor.rCampo(tcStr, 'TelefonePrestador');
   NFSe.Tomador.Contato.Telefone          := Leitor.rCampo(tcStr, 'DDDTomador') + Leitor.rCampo(tcStr, 'TelefoneTomador');

   NFSE.MotivoCancelamento                := Leitor.rCampo(tcStr, 'MotCancelamento');

   NFSe.IntermediarioServico.CpfCnpj      := Leitor.rCampo(tcStr, 'CPFCNPJIntermediario');


   if (Leitor.rExtrai(1, 'Deducoes') <> '') then
   begin
     Item := 0 ;
     while (Leitor.rExtrai(1, 'Deducao', '', Item + 1) <> '') do
     begin
       FNfse.Servico.Deducao.Add;
       FNfse.Servico.Deducao[Item].DeducaoPor  :=
          StrToEnumerado( ok,Leitor.rCampo(tcStr, 'DeducaoPor'),
                          ['','Percentual','Valor'],
                          [ dpNenhum,dpPercentual, dpValor ]);

       FNfse.Servico.Deducao[Item].TipoDeducao :=
          StrToEnumerado( ok,Leitor.rCampo(tcStr, 'TipoDeducao'),
                          ['', 'Despesas com Materiais', 'Despesas com Sub-empreitada'],
                          [ tdNenhum, tdMateriais, tdSubEmpreitada ]);

       FNfse.Servico.Deducao[Item].CpfCnpjReferencia := Leitor.rCampo(tcStr, 'CPFCNPJReferencia');
       FNfse.Servico.Deducao[Item].NumeroNFReferencia := Leitor.rCampo(tcStr, 'NumeroNFReferencia');
       FNfse.Servico.Deducao[Item].ValorTotalReferencia := Leitor.rCampo(tcDe2, 'ValorTotalReferencia');
       FNfse.Servico.Deducao[Item].PercentualDeduzir := Leitor.rCampo(tcDe2, 'PercentualDeduzir');
       FNfse.Servico.Deducao[Item].ValorDeduzir := Leitor.rCampo(tcDe2, 'ValorDeduzir');
       inc(Item);
     end;
   end;

   if (Leitor.rExtrai(1, 'Itens') <> '') then
   begin
     Item := 0 ;
     while (Leitor.rExtrai(1, 'Item', '', Item + 1) <> '') do
     begin
       FNfse.Servico.ItemServico.Add;
       FNfse.Servico.ItemServico[Item].Descricao     := Leitor.rCampo(tcStr, 'DiscriminacaoServico');
       FNfse.Servico.ItemServico[Item].Quantidade    := Leitor.rCampo(tcDe2, 'Quantidade');
       FNfse.Servico.ItemServico[Item].ValorUnitario := Leitor.rCampo(tcDe2, 'ValorUnitario');
       FNfse.Servico.ItemServico[Item].ValorTotal    := Leitor.rCampo(tcDe2, 'ValorTotal');
       FNfse.Servico.ItemServico[Item].Tributavel    := StrToEnumerado( ok,Leitor.rCampo(tcStr, 'Tributavel'), ['N','S'], [ snNao, snSim ]);
       FNfse.Servico.Valores.ValorServicos           := (FNfse.Servico.Valores.ValorServicos + FNfse.Servico.ItemServico[Item].ValorTotal);
       inc(Item);
     end;
   end;
  end;

  FNfse.Servico.Valores.ValorIss                          := (FNfse.Servico.Valores.ValorServicos * NFSe.Servico.Valores.Aliquota)/100;
  FNFSe.Servico.Valores.ValorLiquidoNfse                  := (FNfse.Servico.Valores.ValorServicos -
                                                             (FNfse.Servico.Valores.ValorDeducoes +
                                                              FNfse.Servico.Valores.DescontoCondicionado+
                                                              FNfse.Servico.Valores.DescontoIncondicionado+
                                                              FNFSe.Servico.Valores.ValorIssRetido));
  FNfse.Servico.Valores.BaseCalculo                       := NFSe.Servico.Valores.ValorLiquidoNfse;

 Result := True;
end;

function TNFSeR.LerNFSe_Infisc: Boolean;
var
  ok  : Boolean;
  Item, posI, count: integer;
  sOperacao, sTributacao: string;
  strItem: ansiString;
  leitorAux: TLeitor;
  dEmi : String;
  hEmi : String;
  dia, mes, ano, hora, minuto: word;
begin
  Leitor.Grupo := Leitor.Arquivo;

  if (Pos('<NFS-e>', Leitor.Arquivo) > 0) then
  begin
     // Alterado Por Moro em 18/02/2015
     VersaoXML:= Leitor.rAtributo('versao');
     if VersaoXML = '1.1' then begin //if Caxias do Sul versão XML 1.1

        // Id
        NFSe.Numero            := Leitor.rCampo(tcStr, 'nNFS-e');
        NFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'cNFS-e');
        NFSe.SeriePrestacao    := Leitor.rCampo(tcStr, 'serie');
        NFSe.Competencia       := Leitor.rCampo(tcStr, 'dEmi');
        dEmi                   := Leitor.rCampo(tcStr, 'dEmi');
        hEmi                   := Leitor.rCampo(tcStr, 'hEmi');

        ano := StrToInt( copy( dEmi, 1 , 4 ) );
        mes := strToInt( copy( dEmi, 6 , 2 ) );
        dia := strToInt( copy( dEmi, 9 , 2 ) );

        hora   := strToInt( Copy( hEmi , 0 , 2 ) );
        minuto := strToInt( copy( hEmi , 4 , 2 ) );

        Nfse.DataEmissao := EncodeDateTime( ano, mes, dia, hora,minuto,0,0);

        NFSe.Status      := StrToEnumerado(ok, Leitor.rCampo(tcStr, 'anulada'),['N','S'],[srNormal, srCancelado]);
        NFSe.InfID.ID    := SomenteNumeros(NFSe.CodigoVerificacao);

        NFSe.ChaveNFSe                   := Leitor.rCampo(tcStr, 'refNF');
        NFSe.Servico.CodigoMunicipio     := Leitor.rCampo(tcStr, 'cMun');
        NFSe.Servico.MunicipioIncidencia := StrToIntDef(NFSe.Servico.CodigoMunicipio,0);
        //NFSe.OutrasInformacoes           := Leitor.rCampo(tcStr, 'xInf');

        NFSe.Servico.ItemListaServico    := Leitor.rCampo(tcStr, 'infAdic');

        // prest
        leitorAux := TLeitor.Create;
        try
          leitorAux.Arquivo := Leitor.rExtrai(1,'prest');
          leitorAux.Grupo   := leitorAux.Arquivo;

          NFSe.Prestador.Cnpj                                             := leitorAux.rCampo(tcStr, 'CNPJ');
          NFSe.Prestador.InscricaoMunicipal                               := leitorAux.rCampo(tcStr, 'IM');
          NFSe.Prestador.InscricaoEstadual                                := leitorAux.rCampo(tcStr, 'IE'); //Alterado por Moro em 27/02/2015
          NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj               := NFSe.Prestador.Cnpj;
          NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := NFSe.Prestador.InscricaoMunicipal;
          NFSe.PrestadorServico.RazaoSocial                               := leitorAux.rCampo(tcStr, 'xNome');
          NFSe.PrestadorServico.NomeFantasia                              := leitorAux.rCampo(tcStr, 'xFant');
          NFSe.PrestadorServico.Endereco.Endereco                         := leitorAux.rCampo(tcStr, 'xLgr');
          NFSe.PrestadorServico.Endereco.Numero                           := leitorAux.rCampo(tcStr, 'nro');
          NFSe.PrestadorServico.Endereco.Complemento                      := leitorAux.rCampo(tcStr, 'xCpl');
          NFSe.PrestadorServico.Endereco.Bairro                           := leitorAux.rCampo(tcStr, 'xBairro');
          NFSe.PrestadorServico.Endereco.CodigoMunicipio                  := leitorAux.rCampo(tcStr, 'cMun');
          NFSe.PrestadorServico.Endereco.xMunicipio            := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));
          NFSe.PrestadorServico.Endereco.UF                    := leitorAux.rCampo(tcStr, 'UF');
          NFSe.PrestadorServico.Endereco.CEP                   := leitorAux.rCampo(tcStr, 'CEP');
          NFSe.PrestadorServico.Contato.Telefone               := leitorAux.rCampo(tcStr, 'fone');
          NFSe.PrestadorServico.Contato.Email                  := leitorAux.rCampo(tcStr, 'xEmail');
        finally
          leitorAux.Free;
        end;

        // Tomador
        leitorAux := TLeitor.Create;
        try
          leitorAux.Arquivo := Leitor.rExtrai(1,'TomS');
          leitorAux.Grupo   := leitorAux.Arquivo;

          NFSe.Tomador.IdentificacaoTomador.CpfCnpj := leitorAux.rCampo(tcStr, 'CNPJ') + leitorAux.rCampo(tcStr, 'CPF');
          NFSe.Tomador.RazaoSocial                  := leitorAux.rCampo(tcStr, 'xNome');
          NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := leitorAux.rCampo(tcStr, 'IM');
          NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual  := leitorAux.rCampo(tcStr, 'IE'); //Alterado por Moro em 27/02/2015
          NFSe.Tomador.Endereco.Endereco                       := leitorAux.rCampo(tcStr, 'xLgr');
          NFSe.Tomador.Endereco.Numero                         := leitorAux.rCampo(tcStr, 'nro');
          NFSe.Tomador.Endereco.Complemento                    := leitorAux.rCampo(tcStr, 'xCpl');
          NFSe.Tomador.Endereco.Bairro                         := leitorAux.rCampo(tcStr, 'xBairro');
          NFSe.Tomador.Endereco.CodigoMunicipio                := leitorAux.rCampo(tcStr, 'cMun');
          NFSe.Tomador.Endereco.xMunicipio                     := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));
          NFSe.Tomador.Endereco.UF                             := leitorAux.rCampo(tcStr, 'UF');
          NFSe.Tomador.Endereco.CEP                            := leitorAux.rCampo(tcStr, 'CEP');
          NFSe.Tomador.Contato.Telefone                        := leitorAux.rCampo(tcStr, 'fone');
          NFSe.Tomador.Contato.Email                           := leitorAux.rCampo(tcStr, 'xEmail');
        finally
          leitorAux.Free;
        end;

        // Detalhes dos serviços
        Item := 0 ;
        while (Leitor.rExtrai(1, 'det', '', Item + 1) <> '') do
        begin
          if Leitor.rExtrai(1, 'serv', '', Item + 1) <> '' then
          begin
            Nfse.Servico.ItemServico.Add;
            Nfse.Servico.ItemServico[Item].Codigo        := Leitor.rCampo(tcStr, 'cServ');
            Nfse.Servico.ItemServico[Item].Descricao     := Leitor.rCampo(tcStr, 'xServ');
            Nfse.Servico.ItemServico[Item].Discriminacao := FNfse.Servico.ItemServico[Item].Codigo+' - '+FNfse.Servico.ItemServico[Item].Descricao;
            Nfse.Servico.ItemServico[Item].Quantidade    := Leitor.rCampo(tcDe4, 'qTrib');
            Nfse.Servico.ItemServico[Item].ValorUnitario := Leitor.rCampo(tcDe3, 'vUnit');
            Nfse.Servico.ItemServico[Item].ValorServicos := Leitor.rCampo(tcDe2, 'vServ');
            Nfse.Servico.ItemServico[Item].DescontoIncondicionado := Leitor.rCampo(tcDe2, 'vDesc');
            // ISSQN
            Nfse.Servico.ItemServico[Item].BaseCalculo := Leitor.rCampo(tcDe2, 'vBCISS');
            Nfse.Servico.ItemServico[Item].Aliquota    := Leitor.rCampo(tcDe2, 'pISS');
            Nfse.Servico.ItemServico[Item].ValorIss    := Leitor.rCampo(tcDe2, 'vISS');
            //Alterado por Moro em 27/02/2015
            // Retenções
            NFSe.Servico.ItemServico.Items[Item].ValorIr     := Leitor.rCampo(tcDe2, 'vRetIR');
            NFSe.Servico.ItemServico.Items[Item].ValorPis    := Leitor.rCampo(tcDe2, 'vRetPISPASEP');
            NFSe.Servico.ItemServico.Items[Item].ValorCofins := Leitor.rCampo(tcDe2, 'vRetCOFINS');
            NFSe.Servico.ItemServico.Items[Item].ValorCsll   := Leitor.rCampo(tcDe2, 'vRetCSLL');
            NFSe.Servico.ItemServico.Items[Item].ValorInss   := Leitor.rCampo(tcDe2, 'vRetINSS');
          end;
          inc(Item);
        end;

        // Total
        leitorAux := TLeitor.Create;
        try
          leitorAux.Arquivo := Leitor.rExtrai(1,'total');
          leitorAux.Grupo   := leitorAux.Arquivo;
          // Valores
          NFSe.Servico.Valores.ValorServicos          := leitorAux.rCampo(tcDe2, 'vServ');
          NFSe.Servico.Valores.DescontoIncondicionado := leitorAux.rCampo(tcDe2, 'vDesc');
          NFSe.Servico.Valores.ValorLiquidoNfse       := leitorAux.rCampo(tcDe2, 'vtLiq');
          // ISSQN
          NFSe.Servico.Valores.BaseCalculo := leitorAux.rCampo(tcDe2, 'vBCISS');
          NFSe.Servico.Valores.ValorIss    := leitorAux.rCampo(tcDe2, 'vISS');
          // ISSQN Retido
          NFSe.Servico.Valores.ValorIssRetido := leitorAux.rCampo(tcDe2, 'vSTISS');
          if NFSe.Servico.Valores.ValorIssRetido > 0 then
          begin
            NFSe.Servico.Valores.IssRetido   := stRetencao;
            NFSe.Servico.MunicipioIncidencia := StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio,0);
          end;
          //Alterado por Moro em 27/02/2015
          // Retenções
          NFSe.Servico.Valores.ValorIr     := Leitor.rCampo(tcDe2, 'vRetIR');
          NFSe.Servico.Valores.ValorPis    := Leitor.rCampo(tcDe2, 'vRetPISPASEP');
          NFSe.Servico.Valores.ValorCofins := Leitor.rCampo(tcDe2, 'vRetCOFINS');
          NFSe.Servico.Valores.ValorCsll   := Leitor.rCampo(tcDe2, 'vRetCSLL');
          NFSe.Servico.Valores.ValorInss   := Leitor.rCampo(tcDe2, 'vRetINSS');
        finally
          leitorAux.Free;
        end;

        //Alterado por Moro em 27/02/2015
        //duplicatas
        Item := 0 ;
        while (Leitor.rExtrai(1, 'faturas', '', Item + 1) <> '') do
        begin
          if Leitor.rExtrai(1, 'fat', '', Item + 1) <> '' then
          begin
            Nfse.CondicaoPagamento.Parcelas.Add;
            Nfse.CondicaoPagamento.Parcelas[Item].Parcela        := Leitor.rCampo(tcStr, 'nFat');
            Nfse.CondicaoPagamento.Parcelas[Item].DataVencimento := Leitor.rCampo(tcDat, 'dVenc');
            Nfse.CondicaoPagamento.Parcelas[Item].Valor          := Leitor.rCampo(tcDe2, 'vFat');
          end;
          inc(Item);
        end;

        Result := True;

     end else //fim if Caxias do Sul

     begin // demais Cidades
        VersaoXML:= '1';

        // Ident.
        NFSe.Numero            := Leitor.rCampo(tcStr, 'nNFS-e');
        NFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'cNFS-e');
        NFSe.SeriePrestacao    := Leitor.rCampo(tcStr, 'serie');
        NFSe.Competencia       := Leitor.rCampo(tcStr, 'dEmi');
        dEmi                   := Leitor.rCampo(tcStr, 'dEmi');
        hEmi                   := Leitor.rCampo(tcStr, 'hEmi');

        ano := StrToInt( copy( dEmi, 1 , 4 ) );
        mes := strToInt( copy( dEmi, 6 , 2 ) );
        dia := strToInt( copy( dEmi, 9 , 2 ) );

        hora   := strToInt( Copy( hEmi , 0 , 2 ) );
        minuto := strToInt( copy( hEmi , 4 , 2 ) );

        // Leandro do Couto em 19/1/2015

        Nfse.DataEmissao := EncodeDateTime( ano, mes, dia, hora,minuto,0,0);
     //   NFSe.DataEmissao       := StrToDateTimeDef(dEmi + ' ' + hEmi, 0);
     //   NFSe.DataEmissao       := StrToDateTimeDef(Leitor.rCampo(tcStr, 'dEmi') + ' ' + Leitor.rCampo(tcStr, 'hEmi'), 0);
        NFSe.Status      := StrToEnumerado(ok, Leitor.rCampo(tcStr, 'anulada'),['N','S'],[srNormal, srCancelado]);
        NFSe.InfID.ID    := SomenteNumeros(NFSe.CodigoVerificacao);

        NFSe.ChaveNFSe                   := Leitor.rCampo(tcStr, 'refNF');
        NFSe.Servico.CodigoMunicipio     := Leitor.rCampo(tcStr, 'cMunFG');
        NFSe.Servico.MunicipioIncidencia := StrToIntDef(NFSe.Servico.CodigoMunicipio,0);
        NFSe.OutrasInformacoes           := Leitor.rCampo(tcStr, 'xInf');

        // Lay-Out Infisc não possui campo específicos
        NFSe.NaturezaOperacao            := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'natOp'));
        NFSe.Servico.ItemListaServico    := Leitor.rCampo(tcStr, 'infAdic');

        // Emit
        leitorAux := TLeitor.Create;
        try
          leitorAux.Arquivo := Leitor.rExtrai(1,'emit');
          leitorAux.Grupo   := leitorAux.Arquivo;

          NFSe.Prestador.Cnpj := leitorAux.rCampo(tcStr, 'CNPJ');
          NFSe.Prestador.InscricaoMunicipal := leitorAux.rCampo(tcStr, 'IM');
          NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := NFSe.Prestador.Cnpj;
          NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := NFSe.Prestador.InscricaoMunicipal;
          NFSe.PrestadorServico.RazaoSocial := leitorAux.rCampo(tcStr, 'xNome');
          NFSe.PrestadorServico.NomeFantasia := leitorAux.rCampo(tcStr, 'xFant');
          NFSe.PrestadorServico.Endereco.Endereco := leitorAux.rCampo(tcStr, 'xLgr');
          NFSe.PrestadorServico.Endereco.Numero := leitorAux.rCampo(tcStr, 'nro');
          NFSe.PrestadorServico.Endereco.Complemento := leitorAux.rCampo(tcStr, 'xCpl');
          NFSe.PrestadorServico.Endereco.Bairro := leitorAux.rCampo(tcStr, 'xBairro');
          NFSe.PrestadorServico.Endereco.CodigoMunicipio := leitorAux.rCampo(tcStr, 'cMun');
          NFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));
          NFSe.PrestadorServico.Endereco.UF := leitorAux.rCampo(tcStr, 'UF');
          NFSe.PrestadorServico.Endereco.CEP := leitorAux.rCampo(tcStr, 'CEP');
          NFSe.PrestadorServico.Contato.Telefone := leitorAux.rCampo(tcStr, 'fone');
          NFSe.PrestadorServico.Contato.Email := leitorAux.rCampo(tcStr, 'xEmail');
        finally
          leitorAux.Free;
        end;

        // Tomador
        leitorAux := TLeitor.Create;
        try
          leitorAux.Arquivo := Leitor.rExtrai(1,'TomS');
          leitorAux.Grupo   := leitorAux.Arquivo;

          NFSe.Tomador.IdentificacaoTomador.CpfCnpj := leitorAux.rCampo(tcStr, 'CNPJ') + leitorAux.rCampo(tcStr, 'CPF');
          NFSe.Tomador.RazaoSocial := leitorAux.rCampo(tcStr, 'xNome');
          NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := leitorAux.rCampo(tcStr, 'IM');
          NFSe.Tomador.Endereco.Endereco := leitorAux.rCampo(tcStr, 'xLgr');
          NFSe.Tomador.Endereco.Numero := leitorAux.rCampo(tcStr, 'nro');
          NFSe.Tomador.Endereco.Complemento := leitorAux.rCampo(tcStr, 'xCpl');
          NFSe.Tomador.Endereco.Bairro := leitorAux.rCampo(tcStr, 'xBairro');
          NFSe.Tomador.Endereco.CodigoMunicipio := leitorAux.rCampo(tcStr, 'cMun');
          NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));
          NFSe.Tomador.Endereco.UF := leitorAux.rCampo(tcStr, 'UF');
          NFSe.Tomador.Endereco.CEP := leitorAux.rCampo(tcStr, 'CEP');
          NFSe.Tomador.Contato.Telefone := leitorAux.rCampo(tcStr, 'fone');
          NFSe.Tomador.Contato.Email := leitorAux.rCampo(tcStr, 'xEmail');
        finally
          leitorAux.Free;
        end;

        // Detalhes dos serviços
        Item := 0 ;
        while (Leitor.rExtrai(1, 'det', '', Item + 1) <> '') do
        begin
          if Leitor.rExtrai(1, 'serv', '', Item + 1) <> '' then
          begin
            Nfse.Servico.ItemServico.Add;
            Nfse.Servico.ItemServico[Item].Codigo        := Leitor.rCampo(tcStr, 'cServ');
            Nfse.Servico.ItemServico[Item].Descricao     := Leitor.rCampo(tcStr, 'xServ');
            Nfse.Servico.ItemServico[Item].Discriminacao := FNfse.Servico.ItemServico[Item].Codigo+' - '+FNfse.Servico.ItemServico[Item].Descricao;
            Nfse.Servico.ItemServico[Item].Quantidade    := Leitor.rCampo(tcDe4, 'qTrib');
            Nfse.Servico.ItemServico[Item].ValorUnitario := Leitor.rCampo(tcDe3, 'vUnit');
            Nfse.Servico.ItemServico[Item].ValorServicos := Leitor.rCampo(tcDe2, 'vServ');
            Nfse.Servico.ItemServico[Item].DescontoIncondicionado := Leitor.rCampo(tcDe2, 'vDesc');
            // ISSQN
            Nfse.Servico.ItemServico[Item].BaseCalculo := Leitor.rCampo(tcDe2, 'vBCISS');
            Nfse.Servico.ItemServico[Item].Aliquota    := Leitor.rCampo(tcDe2, 'pISS');
            Nfse.Servico.ItemServico[Item].ValorIss    := Leitor.rCampo(tcDe2, 'vISS');
            // Retenções
            NFSe.Servico.ItemServico.Items[Item].ValorIr     := Leitor.rCampo(tcDe2, 'vRetIRF');
            NFSe.Servico.ItemServico.Items[Item].ValorPis    := Leitor.rCampo(tcDe2, 'vRetLei10833-PIS-PASEP');
            NFSe.Servico.ItemServico.Items[Item].ValorCofins := Leitor.rCampo(tcDe2, 'vRetLei10833-COFINS');
            NFSe.Servico.ItemServico.Items[Item].ValorCsll   := Leitor.rCampo(tcDe2, 'vRetLei10833-CSLL');
            NFSe.Servico.ItemServico.Items[Item].ValorInss   := Leitor.rCampo(tcDe2, 'vRetINSS');
          end;
          inc(Item);
        end;

        // Total
        leitorAux := TLeitor.Create;
        try
          leitorAux.Arquivo := Leitor.rExtrai(1,'total');
          leitorAux.Grupo   := leitorAux.Arquivo;
          // Valores
          NFSe.Servico.Valores.ValorServicos          := leitorAux.rCampo(tcDe2, 'vServ');
          NFSe.Servico.Valores.DescontoIncondicionado := leitorAux.rCampo(tcDe2, 'vDesc');
          NFSe.Servico.Valores.ValorLiquidoNfse       := leitorAux.rCampo(tcDe2, 'vtLiq');
          // ISSQN
          NFSe.Servico.Valores.BaseCalculo := leitorAux.rCampo(tcDe2, 'vBCISS');
          NFSe.Servico.Valores.ValorIss    := leitorAux.rCampo(tcDe2, 'vISS');
          // ISSQN Retido
          NFSe.Servico.Valores.ValorIssRetido := leitorAux.rCampo(tcDe2, 'vSTISS');
          if NFSe.Servico.Valores.ValorIssRetido > 0 then
          begin
            NFSe.Servico.Valores.IssRetido   := stRetencao;
            NFSe.Servico.MunicipioIncidencia := StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio,0);
          end;
          // Retenções
          NFSe.Servico.Valores.ValorIr     := Leitor.rCampo(tcDe2, 'vRetIRF');
          NFSe.Servico.Valores.ValorPis    := Leitor.rCampo(tcDe2, 'vRetLei10833-PIS-PASEP');
          NFSe.Servico.Valores.ValorCofins := Leitor.rCampo(tcDe2, 'vRetLei10833-COFINS');
          NFSe.Servico.Valores.ValorCsll   := Leitor.rCampo(tcDe2, 'vRetLei10833-CSLL');
          NFSe.Servico.Valores.ValorInss   := Leitor.rCampo(tcDe2, 'vRetINSS');
        finally
          leitorAux.Free;
        end;

        Result := True;
     end; //fim if demais cidades
  end;
end;

function TNFSeR.LerNFSe_Equiplano: Boolean;
begin
 // Falta Implementar

 Result := True;
end;

function TNFSeR.LerNFSe: Boolean;
var
 ok: Boolean;
 CM: String;
begin

 if FProvedor = proNenhum then
 begin
   if (Leitor.rExtrai(1, 'OrgaoGerador') <> '') then
   begin
     CM := Leitor.rCampo(tcStr, 'CodigoMunicipio');
     FProvedor := StrToProvedor(Ok, CodCidadeToProvedor(StrToIntDef(CM, 0)));
   end;

   if (CM = '') or (CM = '0') then
   begin
     if (Leitor.rExtrai(1, 'Servico') <> '') then
     begin
       CM := Leitor.rCampo(tcStr, 'CodigoMunicipio');
       FProvedor := StrToProvedor(Ok, CodCidadeToProvedor(StrToIntDef(CM, 0)));
     end;
   end;

   if (CM = '') or (CM = '0') then
   begin
     if (Leitor.rExtrai(1, 'PrestadorServico') <> '') then
     begin
       CM := OnlyNumber(Leitor.rCampo(tcStr, 'CodigoMunicipio'));
       if CM = '' then
         CM := Leitor.rCampo(tcStr, 'Cidade');
       FProvedor := StrToProvedor(Ok, CodCidadeToProvedor(StrToIntDef(CM, 0)));
     end
     else
       FProvedor := proNenhum;
   end;
   { Alterado Por Cleiver em - 22-08-2014 }
   if (FProvedor = proNenhum) and (Pos('https://nfse.goiania.go.gov.br/ws/', Leitor.Arquivo) > 0)  then
     FProvedor := proGoiania;
 end;

 if (Leitor.rExtrai(1, 'Nfse') <> '') or (Pos('Nfse versao="2.01"', Leitor.Arquivo) > 0) then
 begin
   if (Leitor.rExtrai(2, 'InfNfse') <> '') or (Leitor.rExtrai(1, 'InfNfse') <> '')
    then begin
     NFSe.Numero            := Leitor.rCampo(tcStr, 'Numero');
     NFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');

     if FProvedor in [proFreire, proSpeedGov, proVitoria, proDBSeller]
      then NFSe.DataEmissao := Leitor.rCampo(tcDat, 'DataEmissao')
      else NFSe.DataEmissao := Leitor.rCampo(tcDatHor, 'DataEmissao');

     // Alterado por Leonardo Gregianin 11/01/2014: Tratar erro de conversão de tipo no Provedor Ábaco
  	 if Leitor.rCampo(tcStr, 'DataEmissaoRps') <> '0000-00-00' then
	     NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissaoRps');

     NFSe.NaturezaOperacao         := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'NaturezaOperacao'));
     NFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
     NFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
     NFSe.Competencia              := Leitor.rCampo(tcStr, 'Competencia');
     NFSe.OutrasInformacoes        := Leitor.rCampo(tcStr, 'OutrasInformacoes');
     NFSe.ValorCredito             := Leitor.rCampo(tcDe2, 'ValorCredito');

     if FProvedor = proVitoria
      then NFSe.IncentivadorCultural := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivoFiscal'))
      else NFSe.IncentivadorCultural := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivadorCultural'));

     if FProvedor = proISSNet
      then FNFSe.NfseSubstituida := ''
      else begin
       NFSe.NfseSubstituida := Leitor.rCampo(tcStr, 'NfseSubstituida');
       if NFSe.NfseSubstituida = ''
        then NFSe.NfseSubstituida := Leitor.rCampo(tcStr, 'NfseSubstituta');
      end;

    end;
 end;

 case FProvedor of
  proABRASFv1,
  proAbaco,
  proBetha,
  proBHISS,
  proDBSeller,
  proFISSLex,
  proGinfes,
  proGovBR,
  proISSCuritiba,
  proISSIntel,
  proISSNet,
  proLexsom,
  proNatal,
  proProdemge,
  proPronim,
  proPublica,
  proRecife,
  proRJ,
  proSimplISS,
  proSJP,
  proSpeedGov,
  proThema,
  proTinus,
  proTiplan,
  proWebISS: Result := LerNFSe_ABRASF_V1;

  proABRASFv2,
  pro4R,
  proActcon,
  proAgili,
  proCoplan,
  proDigifred,
  proFIntelISS,
  proFiorilli,
  proFreire,
  proGoiania,
  proGovDigital,
  proISSDigital,
  proISSe,
  proLink3,
  proMitra,
  proNFSeBrasil,
  proProdata,
  proPVH,
  proSaatri,
  proSisPMJP,
  proSystemPro,
  proTecnos,
  ProVirtual,
  proVitoria: Result := LerNFSe_ABRASF_V2;

  proInfisc:  Result := LerNFSe_Infisc;

  proIssDsf:  Result := LerNFSe_IssDsf;

  //  proSystemPro: Result := LerNFSe_ProvedorSysPro;

  proEquiplano: Result := LerNFSe_Equiplano;

  else Result := False;
 end;

 if Leitor.rExtrai(1, 'NfseCancelamento') <> ''
  then begin
   NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHora');
   if NFSe.NfseCancelamento.DataHora = 0
    then NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHoraCancelamento');
   NFSe.NfseCancelamento.Pedido.CodigoCancelamento := Leitor.rCampo(tcStr, 'CodigoCancelamento');
  end;
end;

function TNFSeR.LerXml: boolean;
begin
 Result := False;

 if Pos('<Nfse', Leitor.Arquivo) > 0
  then Result := LerNFSe                                              { Alterado Por Cleiver em - 22-08-2014 }
  else if (Pos('<Rps', Leitor.Arquivo) > 0) or (Pos('<rps', Leitor.Arquivo) > 0) or (Pos('<RPS', Leitor.Arquivo) > 0)
        then Result := LerRPS
        else if (Pos('<Notas>', Leitor.Arquivo) > 0) or (Pos('<Nota>', Leitor.Arquivo) > 0)
              then Result := LerNFSe_IssDSF
              else if (Pos('<NFS-e>', Leitor.Arquivo) > 0)
                    then Result := LerNFSe_Infisc;

 // Grupo da TAG <signature> ***************************************************
 Leitor.Grupo := Leitor.Arquivo;

 NFSe.signature.URI             := Leitor.rAtributo('Reference URI=');
 NFSe.signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
 NFSe.signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
 NFSe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
end;

function TNFSeR.LerNFSe_ProvedorSysPro: Boolean;
var
  Item, I: integer;
 	ok  : Boolean;
//  dt, d, m , a : string;
begin
  result:=True;

  if (Leitor.rExtrai(1, 'InfDeclaracaoPrestacaoServico') <> '') then
  begin
   NFSe.Competencia              := Leitor.rCampo(tcStr, 'Competencia');
   NFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
   NFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
   NFSe.IncentivadorCultural     := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivoFiscal'));
   NFSe.Producao                 := StrToSimNao(ok, Leitor.rCampo(tcStr, 'Producao'));
   NFSe.InfID.ID                 := Leitor.rAtributo('InfNfse Id=');

   if (Leitor.rExtrai(1, 'PrestadorServico') <> '') then
   begin
     NFSe.PrestadorServico.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');
     NFSe.PrestadorServico.NomeFantasia := Leitor.rCampo(tcStr, 'NomeFantasia');
     NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

     // Endereço
     NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
     if Copy(NFSe.PrestadorServico.Endereco.Endereco, 1, 10) = '<' + 'Endereco>' then
       NFSe.PrestadorServico.Endereco.Endereco := Copy(NFSe.PrestadorServico.Endereco.Endereco, 11, 125);
     NFSe.PrestadorServico.Endereco.Numero := Leitor.rCampo(tcStr, 'Numero');
     NFSe.PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
     NFSe.PrestadorServico.Endereco.Bairro := Leitor.rCampo(tcStr, 'Bairro');
     NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
       NFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));
     NFSe.PrestadorServico.Endereco.UF := Leitor.rCampo(tcStr, 'UF');
     NFSe.PrestadorServico.Endereco.CodigoPais := Leitor.rCampo(tcStr, 'CodigoPais');
     NFSe.PrestadorServico.Endereco.Cep := Leitor.rCampo(tcStr, 'Cep');

     // Contato
     NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
     NFSe.PrestadorServico.Contato.Email := Leitor.rCampo(tcStr, 'Email');

     // CNPJ/CPF
     if Leitor.rExtrai(3, 'CpfCnpj') <> '' then
     begin
       NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cpf');
       if NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = '' then
         NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
     end
     else
       NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
    end; // fim Prestador

   if (Leitor.rExtrai(1, 'ValoresNfse') <> '') then
   begin
     NFSe.Servico.Valores.ValorServicos    := Leitor.rCampo(tcDe2, 'ValorServicos');
     NFSe.Servico.Valores.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');
     NFSe.Servico.Valores.ValorIss         := Leitor.rCampo(tcDe2, 'ValorIss');
   end;

   if (Leitor.rExtrai(1, 'Rps') <> '') then
   begin
     NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissao');
     NFSe.Status         := StrToStatusRPS(ok, Leitor.rCampo(tcStr, 'Status'));

     if (Leitor.rExtrai(2, 'IdentificacaoRps') <> '') then
     begin
       NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
       NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
       NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
       NFSe.InfID.ID                := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
     end;
   end; // fim Rps


   if (Leitor.rExtrai(1, 'Servico') <> '')
    then begin
     NFSe.Servico.Valores.IssRetido      := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
     NFSe.Servico.Valores.ValorIssRetido := 0;
     NFSe.Servico.ResponsavelRetencao    := StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'ResponsavelRetencao'));
     NFSe.Servico.ItemListaServico       := OnlyNumber(Leitor.rCampo(tcStr, 'ItemListaServico'));

     Item := StrToInt(OnlyNumber(Nfse.Servico.ItemListaServico));
     if Item<100 then
       Item:=Item*100+1;

     NFSe.Servico.ItemListaServico := FormatFloat('0000', Item);
     NFSe.Servico.ItemListaServico := Copy(NFSe.Servico.ItemListaServico, 1, 2) + '.' +
                                      Copy(NFSe.Servico.ItemListaServico, 3, 2);

     NFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(NFSe.Servico.ItemListaServico));

     NFSe.Servico.Discriminacao       := Leitor.rCampo(tcStr, 'Discriminacao');
     NFSe.Servico.CodigoMunicipio     := Leitor.rCampo(tcStr, 'CodigoMunicipio');
     NFSe.Servico.CodigoPais          := Leitor.rCampo(tcInt, 'CodigoPais');
     NFSe.Servico.ExigibilidadeISS    := StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'ExigibilidadeISS'));
     NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'MunicipioIncidencia');

     NFSe.Servico.Valores.Aliquota    := Leitor.rCampo(tcDe3, 'Aliquota');

     // Itens
     for I := 1 to 200 do
     begin
       if (Leitor.rExtrai(1, 'Servico', 'Servico', i) <> '') then
       begin

         with NFSe.Servico.ItemServico.Add do
         begin
           Descricao       := Leitor.rCampo(tcStr, 'Discriminacao');
           Discriminacao   := Leitor.rCampo(tcStr, 'Discriminacao');

           //if (Leitor.rExtrai(3, 'Valores') <> '') then
           if (Leitor.rExtrai(1,'Valores','Valores',i) <> '') then
           begin
             ValorServicos := Leitor.rCampo(tcDe2, 'ValorServicos');
             ValorDeducoes := Leitor.rCampo(tcDe2, 'ValorDeducoes');
             ValorIss      := Leitor.rCampo(tcDe2, 'ValorIss');
             Aliquota      := Leitor.rCampo(tcDe3, 'Aliquota');
             DescontoCondicionado   := Leitor.rCampo(tcDe2, 'DescontoCondicionado');
             DescontoIncondicionado := Leitor.rCampo(tcDe2, 'DescontoIncondicionado');

             // Valores ISS
             BaseCalculo   := (ValorServicos - ValorDeducoes - DescontoIncondicionado);
             ValorPis      := Leitor.rCampo(tcDe2, 'ValorPis');
             ValorCofins   := Leitor.rCampo(tcDe2, 'ValorCofins');
             ValorInss     := Leitor.rCampo(tcDe2, 'ValorInss');
             ValorIr       := Leitor.rCampo(tcDe2, 'ValorIr');
             ValorCsll     := Leitor.rCampo(tcDe2, 'ValorCsll');

             // ISS Retido
             if NFSe.Servico.Valores.IssRetido=stRetencao then
               NFSe.Servico.Valores.ValorIssRetido:=NFSe.Servico.Valores.ValorIssRetido+ValorIss;
           end;
         end;
       end
       else
         Break;
     end;

   end; // fim lista serviço

   if (Leitor.rExtrai(1, 'Tomador') <> '')
    then begin
     NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');
     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
     NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual  := Leitor.rCampo(tcStr, 'InscricaoEstadual');

     if Leitor.rCampo(tcStr, 'Cpf')<>''
      then NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
      else NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
     NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
     if Copy(NFSe.Tomador.Endereco.Endereco, 1, 10) = '<' + 'Endereco>'
      then NFSe.Tomador.Endereco.Endereco := Copy(NFSe.Tomador.Endereco.Endereco, 11, 125);

     NFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
     NFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
     NFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

     if VersaoXML='1'
      then begin
       NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
       NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
      end
      else begin
       NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
       NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
      end;

     NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

     if length(NFSe.Tomador.Endereco.CodigoMunicipio)<7
      then NFSe.Tomador.Endereco.CodigoMunicipio := Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
       FormatFloat('00000', StrToIntDef(Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

     if NFSe.Tomador.Endereco.UF = ''
      then NFSe.Tomador.Endereco.UF := NFSe.PrestadorServico.Endereco.UF;

     NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));

     NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
     NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
    end; // fim Tomador

  end; // fim InfDeclaracaoPrestacaoServico
end;

end.
