{$I ACBr.inc}

unit pcnCIOTW_eFrete;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  synacode, ACBrConsts,
  pcnCIOTW, pcnCIOTR,
  pcnAuxiliar, pcnConversao, pcnGerador, pcnLeitor,
  pcnCIOT, pcnConversaoCIOT, pcnConsts;

type
  { TCIOTW_eFrete }

  TCIOTW_eFrete = class(TCIOTWClass)
  private
    FVersaoDF: TVersaoCIOT;
    versao: Integer;
  protected
    procedure GerarIdentificacao(aVersao: Integer);

    procedure GerarGravarProprietario;
    procedure GerarGravarVeiculo;
    procedure GerarGravarMotorista;

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
    procedure GerarRemetente;
    procedure GerarProprietarioCarga;
    procedure GerarVeiculos(xPrefixo: String);

    procedure GerarViagemAdicViagem;
    procedure GerarPagamentosAdicViagem;

    procedure GerarPagamentosAdicPagamento;

    procedure GerarViagemEncerramento;
    procedure GerarPagamentosEncerramento;
    procedure GerarImpostosEncerramento;
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

implementation

uses
  ACBrUtil;

constructor TCIOTW_eFrete.Create(ACIOTW: TCIOTW);
begin
  inherited Create(ACIOTW);
end;

function TCIOTW_eFrete.ObterNomeArquivo: String;
begin
//  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

procedure TCIOTW_eFrete.GerarIdentificacao(aVersao: Integer);
var
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := 'obj1:';
  versao := aVersao;

  //ver de que forma pegar o tocken em caso de não usar certificado
  Gerador.wCampoNFSe(tcStr, 'AP03', 'Token', 01, 01, 0, CIOT.Integradora.Token);
  Gerador.wCampoNFSe(tcStr, 'AP04', 'Integrador', 01, 01, 0, CIOT.Integradora.Integrador);
  Gerador.wCampoNFSe(tcInt, 'AP05', 'Versao', 01, 01, 1, aversao);

  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarGravarProprietario;
begin
  with CIOT.GravarProprietario do
  begin
    Gerador.wCampoNFSe(tcStr, 'AP06', 'CNPJ       ', 01, 01, 1, CNPJ);
    Gerador.wCampoNFSe(tcStr, 'AP07', 'TipoPessoa ', 01, 01, 1, TipoPessoaToStr(TipoPessoa));
    Gerador.wCampoNFSe(tcStr, 'AP08', 'RazaoSocial', 01, 01, 0, RazaoSocial);
    Gerador.wCampoNFSe(tcStr, 'AP09', 'RNTRC      ', 01, 01, 1, RNTRC);

    with Endereco do
    begin
      if CodigoMunicipio > 0 then
      begin
        Gerador.wGrupoNFSe('Endereco', 'AP11');
        Gerador.Prefixo := 'obj1:';
        Gerador.wCampoNFSe(tcStr, 'AP12', 'Bairro         ', 01, 01, 0, Bairro);
        Gerador.wCampoNFSe(tcStr, 'AP13', 'Rua            ', 01, 01, 0, Rua);
        Gerador.wCampoNFSe(tcStr, 'AP14', 'Numero         ', 01, 01, 0, Numero);
        Gerador.wCampoNFSe(tcStr, 'AP15', 'Complemento    ', 01, 01, 0, Complemento);
        Gerador.wCampoNFSe(tcStr, 'AP16', 'CEP            ', 08, 08, 0, CEP);
        Gerador.wCampoNFSe(tcInt, 'AP17', 'CodigoMunicipio', 07, 07, 1, CodigoMunicipio);
        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('/Endereco');
      end;
    end;

    with Telefones do
    begin
      if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
      begin
        Gerador.wGrupoNFSe('Telefones', 'AP18');

        Gerador.Prefixo := 'obj1:';
        if Celular.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Celular', 'AP19');
          Gerador.wCampoNFSe(tcInt, 'AP20', 'DDD   ', 01, 02, 1, Celular.DDD, '');
          Gerador.wCampoNFSe(tcInt, 'AP21', 'Numero', 08, 09, 1, Celular.Numero, '');
          Gerador.wGrupoNFSe('/Celular');
        end;

        if Fixo.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Fixo', 'AP22');
          Gerador.wCampoNFSe(tcInt, 'AP23', 'DDD   ', 01, 02, 1, Fixo.DDD, '');
          Gerador.wCampoNFSe(tcInt, 'AP24', 'Numero', 08, 09, 1, Fixo.Numero, '');
          Gerador.wGrupoNFSe('/Fixo');
        end;

        if Fax.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Fax', 'AP25');
          Gerador.wCampoNFSe(tcInt, 'AP26', 'DDD   ', 01, 02, 1, Fax.DDD, '');
          Gerador.wCampoNFSe(tcInt, 'AP27', 'Numero', 08, 09, 1, Fax.Numero, '');
          Gerador.wGrupoNFSe('/Fax');
        end;

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('/Telefones');
      end;
    end;
  end;
end;

procedure TCIOTW_eFrete.GerarGravarVeiculo;
begin
  with CIOT.GravarVeiculo do
  begin
    if Renavam <> '' then
    begin
      Gerador.wGrupoNFSe('Veiculo', 'AP06');

      Gerador.wCampoNFSe(tcStr, 'AP07', 'Placa          ', 01, 01, 0, Placa);
      Gerador.wCampoNFSe(tcStr, 'AP08', 'Renavam        ', 01, 01, 1, Renavam);
      Gerador.wCampoNFSe(tcStr, 'AP09', 'Chassi         ', 01, 01, 0, Chassi);
      Gerador.wCampoNFSe(tcStr, 'AP10', 'RNTRC          ', 01, 01, 1, RNTRC);
      Gerador.wCampoNFSe(tcInt, 'AP11', 'NumeroDeEixos  ', 01, 01, 1, NumeroDeEixos);
      Gerador.wCampoNFSe(tcInt, 'AP12', 'CodigoMunicipio', 01, 01, 1, CodigoMunicipio);
      Gerador.wCampoNFSe(tcStr, 'AP13', 'Marca          ', 01, 01, 0, Marca);
      Gerador.wCampoNFSe(tcStr, 'AP14', 'Modelo         ', 01, 01, 0, Modelo);
      Gerador.wCampoNFSe(tcInt, 'AP15', 'AnoFabricacao  ', 01, 01, 1, AnoFabricacao);
      Gerador.wCampoNFSe(tcInt, 'AP16', 'AnoModelo      ', 01, 01, 1, AnoModelo);
      Gerador.wCampoNFSe(tcStr, 'AP17', 'Cor            ', 01, 01, 0, Cor);
      Gerador.wCampoNFSe(tcInt, 'AP18', 'Tara           ', 01, 01, 1, Tara);
      Gerador.wCampoNFSe(tcInt, 'AP19', 'CapacidadeKg   ', 01, 01, 1, CapacidadeKg);
      Gerador.wCampoNFSe(tcInt, 'AP20', 'CapacidadeM3   ', 01, 01, 1, CapacidadeM3);
      Gerador.wCampoNFSe(tcStr, 'AP21', 'TipoRodado     ', 01, 20, 1, TipoRodadoToStr(TipoRodado));
      Gerador.wCampoNFSe(tcStr, 'AP22', 'TipoCarroceria ', 01, 20, 1, TipoCarroceriaToStr(TipoCarroceria));

      Gerador.wGrupoNFSe('/Veiculo');
    end;
  end;
end;

procedure TCIOTW_eFrete.GerarGravarMotorista;
begin
  with CIOT.GravarMotorista do
  begin
    Gerador.wCampoNFSe(tcStr, 'AP06', 'CPF                ', 01, 01, 1, CPF);
    Gerador.wCampoNFSe(tcStr, 'AP07', 'Nome               ', 01, 01, 0, Nome);
    Gerador.wCampoNFSe(tcStr, 'AP08', 'CNH                ', 01, 01, 1, CNH);
    Gerador.wCampoNFSe(tcDat, 'AP09', 'DataNascimento     ', 01, 01, 1, DataNascimento);
    Gerador.wCampoNFSe(tcStr, 'AP10', 'NomeDeSolteiraDaMae', 01, 01, 0, NomeDeSolteiraDaMae);

    with Endereco do
    begin
      if CodigoMunicipio > 0 then
      begin
        Gerador.wGrupoNFSe('Endereco', 'AP11');
        Gerador.Prefixo := 'obj1:';
        Gerador.wCampoNFSe(tcStr, 'AP12', 'Bairro         ', 01, 01, 0, Bairro);
        Gerador.wCampoNFSe(tcStr, 'AP13', 'Rua            ', 01, 01, 0, Rua);
        Gerador.wCampoNFSe(tcStr, 'AP14', 'Numero         ', 01, 01, 0, Numero);
        Gerador.wCampoNFSe(tcStr, 'AP15', 'Complemento    ', 01, 01, 0, Complemento);
        Gerador.wCampoNFSe(tcStr, 'AP16', 'CEP            ', 08, 08, 0, CEP);
        Gerador.wCampoNFSe(tcInt, 'AP17', 'CodigoMunicipio', 07, 07, 1, CodigoMunicipio);
        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('/Endereco');
      end;
    end;

    with Telefones do
    begin
      if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
      begin
        Gerador.wGrupoNFSe('Telefones', 'AP18');

        Gerador.Prefixo := 'obj1:';
        if Celular.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Celular', 'AP19');
          Gerador.wCampoNFSe(tcInt, 'AP20', 'DDD   ', 01, 02, 1, Celular.DDD, '');
          Gerador.wCampoNFSe(tcInt, 'AP21', 'Numero', 08, 09, 1, Celular.Numero, '');
          Gerador.wGrupoNFSe('/Celular');
        end;

        if Fixo.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Fixo', 'AP22');
          Gerador.wCampoNFSe(tcInt, 'AP23', 'DDD   ', 01, 02, 1, Fixo.DDD, '');
          Gerador.wCampoNFSe(tcInt, 'AP24', 'Numero', 08, 09, 1, Fixo.Numero, '');
          Gerador.wGrupoNFSe('/Fixo');
        end;

        if Fax.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Fax', 'AP25');
          Gerador.wCampoNFSe(tcInt, 'AP26', 'DDD   ', 01, 02, 1, Fax.DDD, '');
          Gerador.wCampoNFSe(tcInt, 'AP27', 'Numero', 08, 09, 1, Fax.Numero, '');
          Gerador.wGrupoNFSe('/Fax');
        end;

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('/Telefones');
      end;
    end;
  end;
end;

procedure TCIOTW_eFrete.GerarViagem;
var
  i, j: Integer;
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := 'adic:';

  for I := 0 to CIOT.AdicionarOperacao.Viagens.Count -1 do
  begin
    Gerador.wGrupoNFSe('Viagens', 'AP12');

    with CIOT.AdicionarOperacao.Viagens.Items[I] do
    begin
      Gerador.wCampoNFSe(tcStr, 'AP13', 'DocumentoViagem       ', 01, 01, 0, DocumentoViagem);
      Gerador.wCampoNFSe(tcInt, 'AP14', 'CodigoMunicipioOrigem ', 01, 07, 1, CodigoMunicipioOrigem);
      Gerador.wCampoNFSe(tcInt, 'AP15', 'CodigoMunicipioDestino', 01, 07, 1, CodigoMunicipioDestino);
      Gerador.wCampoNFSe(tcStr, 'AP16', 'CepOrigem             ', 01, 01, 0, CepOrigem);
      Gerador.wCampoNFSe(tcStr, 'AP17', 'CepDestino            ', 01, 01, 0, CepDestino);
      Gerador.wCampoNFSe(tcInt, 'AP18', 'DistanciaPercorrida   ', 01, 07, 1, DistanciaPercorrida);

      Gerador.Prefixo := 'obj:';

      if Valores.TotalOperacao > 0 then
      begin
        Gerador.wGrupoNFSe('Valores', 'AP19');

        with Valores do
        begin
          Gerador.wCampoNFSe(tcDe2, 'AP20', 'TotalOperacao              ', 01, 01, 1, TotalOperacao);
          Gerador.wCampoNFSe(tcDe2, 'AP21', 'TotalViagem                ', 01, 01, 1, TotalViagem);
          Gerador.wCampoNFSe(tcDe2, 'AP22', 'TotalDeAdiantamento        ', 01, 01, 1, TotalDeAdiantamento);
          Gerador.wCampoNFSe(tcDe2, 'AP23', 'TotalDeQuitacao            ', 01, 01, 1, TotalDeQuitacao);
          Gerador.wCampoNFSe(tcDe2, 'AP24', 'Combustivel                ', 01, 01, 1, Combustivel);
          Gerador.wCampoNFSe(tcDe2, 'AP25', 'Pedagio                    ', 01, 01, 1, Pedagio);
          Gerador.wCampoNFSe(tcDe2, 'AP26', 'OutrosCreditos             ', 01, 01, 1, OutrosCreditos);
          Gerador.wCampoNFSe(tcStr, 'AP27', 'JustificativaOutrosCreditos', 01, 01, 0, JustificativaOutrosCreditos);
          Gerador.wCampoNFSe(tcDe2, 'AP28', 'Seguro                     ', 01, 01, 1, Seguro);
          Gerador.wCampoNFSe(tcDe2, 'AP29', 'OutrosDebitos              ', 01, 01, 1, OutrosDebitos);
          Gerador.wCampoNFSe(tcStr, 'AP30', 'JustificativaOutrosDebitos ', 01, 01, 0, JustificativaOutrosDebitos);
        end;

        Gerador.wGrupoNFSe('/Valores');
      end;

      Gerador.wCampoNFSe(tcStr, 'AP31', 'TipoPagamento', 001, 020, 1, TpPagamentoToStr(TipoPagamento));

      with InformacoesBancarias do
      begin
        if (InstituicaoBancaria <> '') or (Agencia <> '') or (Conta <> '') then
        begin
          Gerador.wGrupoNFSe('InformacoesBancarias', 'AP32');

          Gerador.wCampoNFSe(tcStr, 'AP33', 'InstituicaoBancaria', 01, 01, 0, InstituicaoBancaria);
          Gerador.wCampoNFSe(tcStr, 'AP34', 'Agencia            ', 01, 01, 0, Agencia);
          Gerador.wCampoNFSe(tcStr, 'AP35', 'Conta              ', 01, 01, 0, Conta);
          Gerador.wCampoNFSe(tcStr, 'AP36', 'TipoConta          ', 01, 15, 1, TipoContaToStr(TipoConta));

          Gerador.wGrupoNFSe('/InformacoesBancarias');
        end;
      end;

      if NotasFiscais.Count > 0 then
      begin
        Gerador.Prefixo := 'adic:';
        Gerador.wGrupoNFSe('NotasFiscais', 'AP37');

        for J := 0 to NotasFiscais.Count -1 do
        begin
          with NotasFiscais.Items[J] do
          begin
            Gerador.wGrupoNFSe('NotaFiscal', 'AP38');
            Gerador.wCampoNFSe(tcStr, 'AP39', 'Numero                            ', 01, 01, 0, Numero);
            Gerador.wCampoNFSe(tcStr, 'AP40', 'Serie                             ', 01, 01, 0, Serie);
            Gerador.wCampoNFSe(tcDat, 'AP41', 'Data                              ', 01, 01, 1, Data);
            Gerador.wCampoNFSe(tcDe2, 'AP42', 'ValorTotal                        ', 01, 01, 1, ValorTotal);
            Gerador.wCampoNFSe(tcDe4, 'AP43', 'ValorDaMercadoriaPorUnidade       ', 01, 01, 1, ValorDaMercadoriaPorUnidade);
            Gerador.wCampoNFSe(tcInt, 'AP44', 'CodigoNCMNaturezaCarga            ', 01, 04, 1, CodigoNCMNaturezaCarga);
            Gerador.wCampoNFSe(tcStr, 'AP45', 'DescricaoDaMercadoria             ', 01, 01, 0, DescricaoDaMercadoria);
            Gerador.wCampoNFSe(tcStr, 'AP46', 'UnidadeDeMedidaDaMercadoria       ', 01, 01, 1, TpUnMedMercToStr(UnidadeDeMedidaDaMercadoria));
            Gerador.wCampoNFSe(tcStr, 'AP47', 'TipoDeCalculo                     ', 01, 01, 1, TpVgTipoCalculoToStr(TipoDeCalculo));
            Gerador.wCampoNFSe(tcDe4, 'AP48', 'ValorDoFretePorUnidadeDeMercadoria', 01, 01, 1, ValorDoFretePorUnidadeDeMercadoria);
            Gerador.wCampoNFSe(tcDe4, 'AP49', 'QuantidadeDaMercadoriaNoEmbarque  ', 01, 01, 1, QuantidadeDaMercadoriaNoEmbarque);

            if ToleranciaDePerdaDeMercadoria.Valor > 0 then
            begin
              Gerador.wGrupoNFSe('ToleranciaDePerdaDeMercadoria', 'AP50');
              Gerador.wCampoNFSe(tcStr, 'AP51', 'Tipo ', 01, 01, 1, TpProporcaoToStr(ToleranciaDePerdaDeMercadoria.Tipo));
              Gerador.wCampoNFSe(tcDe2, 'AP52', 'Valor', 01, 01, 1, ToleranciaDePerdaDeMercadoria.Valor);
              Gerador.wGrupoNFSe('/ToleranciaDePerdaDeMercadoria');
            end;

            if DiferencaDeFrete.Tipo <> SemDiferenca then
            begin
              Gerador.wGrupoNFSe('DiferencaDeFrete', 'AP53');
              Gerador.Prefixo := 'obj:';

              Gerador.wCampoNFSe(tcStr, 'AP50', 'Tipo', 01, 01, 1, TpDifFreteToStr(DiferencaDeFrete.Tipo));
              Gerador.wCampoNFSe(tcStr, 'AP51', 'Base', 01, 01, 1, TpDiferencaFreteBCToStr(DiferencaDeFrete.Base));

              if DiferencaDeFrete.Tolerancia.Valor > 0 then
              begin
                Gerador.wGrupoNFSe('Tolerancia', 'AP52');
                Gerador.wCampoNFSe(tcStr, 'AP53', 'Tipo ', 01, 01, 1, TpProporcaoToStr(DiferencaDeFrete.Tolerancia.Tipo));
                Gerador.wCampoNFSe(tcDe2, 'AP54', 'Valor', 01, 01, 1, DiferencaDeFrete.Tolerancia.Valor);
                Gerador.wGrupoNFSe('/Tolerancia');
              end;

              if DiferencaDeFrete.MargemGanho.Valor > 0 then
              begin
                Gerador.wGrupoNFSe('MargemGanho', 'AP55');
                Gerador.wCampoNFSe(tcStr, 'AP56', 'Tipo ', 01, 01, 1, TpProporcaoToStr(DiferencaDeFrete.MargemGanho.Tipo));
                Gerador.wCampoNFSe(tcDe2, 'AP57', 'Valor', 01, 01, 1, DiferencaDeFrete.MargemGanho.Valor);
                Gerador.wGrupoNFSe('/MargemGanho');
              end;

              if DiferencaDeFrete.MargemPerda.Valor > 0 then
              begin
                Gerador.wGrupoNFSe('MargemPerda', 'AP58');
                Gerador.wCampoNFSe(tcStr, 'AP59', 'Tipo', 01, 01, 1, TpProporcaoToStr(DiferencaDeFrete.MargemPerda.Tipo));
                Gerador.wCampoNFSe(tcDe2, 'AP60', 'Valor', 01, 01, 1, DiferencaDeFrete.MargemPerda.Valor);
                Gerador.wGrupoNFSe('/MargemPerda');
              end;

              Gerador.Prefixo := 'adic:';
              Gerador.wGrupoNFSe('/DiferencaDeFrete');
            end;
            Gerador.wGrupoNFSe('/NotaFiscal');
          end;
        end;
        Gerador.wGrupoNFSe('/NotasFiscais');
      end;
    end;

    Gerador.wGrupoNFSe('/Viagens');
  end;

  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarImpostos;
begin
  with CIOT.AdicionarOperacao.Impostos do
  begin
    Gerador.wGrupoNFSe('Impostos', 'AP61');
    Gerador.wCampoNFSe(tcDe2, 'AP62', 'IRRF                   ', 01, 01, 1, IRRF, 'Valor destinado ao IRRF');
    Gerador.wCampoNFSe(tcDe2, 'AP63', 'SestSenat              ', 01, 01, 1, SestSenat, 'Valor destinado ao SEST / SENAT');
    Gerador.wCampoNFSe(tcDe2, 'AP64', 'INSS                   ', 01, 01, 1, INSS, 'Valor destinado ao INSS.');
    Gerador.wCampoNFSe(tcDe2, 'AP65', 'ISSQN                  ', 01, 01, 1, ISSQN, 'Valor destinado ao ISSQN.');
    Gerador.wCampoNFSe(tcDe2, 'AP66', 'OutrosImpostos         ', 01, 01, 1, OutrosImpostos, 'Valor destinado a outros impostos não previstos.');
    Gerador.wCampoNFSe(tcStr, 'AP67', 'DescricaoOutrosImpostos', 01, 01, 0, DescricaoOutrosImpostos);
    Gerador.wGrupoNFSe('/Impostos');
  end;
end;

procedure TCIOTW_eFrete.GerarPagamentos;
var
  i: Integer;
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := 'adic:';

  //Pagamentos registrados.
  //- Pode existir mais de 1 pagamento com uma mesma categoria (exceto para Quitacao).
  //- A soma dos pagamentos c/ categoria Adiantamento, deverá ter o mesmo valor apontado na
  //     tag TotalAdiantamento da tag Viagem/Valores, e neste caso, a tag Documento do pagamento
  //     deverá conter o mesmo valor da tag DocumentoViagem da tag Viagem .
  //- Se a viagem possuir a tag TotalQuitacao maior que zero, deverá ter um pagamento correspondente,
  //     com Categoria Quitacao e com o Documento o mesmo valor apontado na tag DocumentoViagem .
  for i := 0 to CIOT.AdicionarOperacao.Pagamentos.Count -1 do
  begin
    with CIOT.AdicionarOperacao.Pagamentos.Items[i] do
    begin
      Gerador.wGrupoNFSe('Pagamentos', 'AP68');
      Gerador.wCampoNFSe(tcStr, 'AP69', 'IdPagamentoCliente', 01, 01, 0, IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente.');
      Gerador.wCampoNFSe(tcDat, 'AP70', 'DataDeLiberacao   ', 01, 01, 1, DataDeLiberacao, 'Data em que o pagamento será liberado para saque.');
      Gerador.wCampoNFSe(tcDe2, 'AP71', 'Valor             ', 01, 01, 1, Valor, 'Valor do pagamento.');

      Gerador.Prefixo := 'obj:';
      Gerador.wCampoNFSe(tcStr, 'AP72', 'TipoPagamento', 01, 01, 1, TpPagamentoToStr(TipoPagamento), 'Tipo de pagamento que será usado pelo contratante. Restrito aos itens da enum: -TransferenciaBancaria -eFRETE');
      Gerador.wCampoNFSe(tcStr, 'AP73', 'Categoria    ', 01, 01, 1, TpCatPagToStr(Categoria), 'Categoria relacionada ao pagamento realizado. Restrita aos membros da ENUM: -Adiantamento, -Estadia, -Quitacao, -SemCategoria, -Frota ');

      Gerador.Prefixo := 'adic:';
      Gerador.wCampoNFSe(tcStr, 'AP74', 'Documento', 01, 01, 0, Documento, 'Documento relacionado a viagem.');

      // Preenchimento obrigatório para o TipoPagamento TransferenciaBancaria.
      // Não deve ser preenchido para TipoPagamento eFRETE.
      with InformacoesBancarias do
      begin
        if (InstituicaoBancaria <> '') or (Agencia <> '') or (Conta <> '') then
        begin
          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('InformacoesBancarias', 'AP75');

          Gerador.wCampoNFSe(tcStr, 'AP76', 'InstituicaoBancaria', 01, 01, 0, InstituicaoBancaria, 'Código de compensação da instituição bancária que será realizado o pagamento. ');
          Gerador.wCampoNFSe(tcStr, 'AP77', 'Agencia            ', 01, 01, 0, Agencia, 'Agência na qual o contratado possui conta com dígito (se houver).');
          Gerador.wCampoNFSe(tcStr, 'AP78', 'Conta              ', 01, 01, 0, Conta, 'Conta do contratado com dígito. ');
          Gerador.wCampoNFSe(tcStr, 'AP79', 'TipoConta          ', 01, 15, 1, TipoContaToStr(TipoConta));

          Gerador.wGrupoNFSe('/InformacoesBancarias');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP80', 'InformacaoAdicional', 01, 01, 0, InformacaoAdicional);

      if Categoria = tcpFrota then
        Gerador.wCampoNFSe(tcStr, 'AP81', 'CnpjFilialAbastecimento', 01, 01, 1, CnpjFilialAbastecimento);

      Gerador.Prefixo := 'adic:';
      Gerador.wGrupoNFSe('/Pagamentos');
    end;
  end;

  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarContratado;
var
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := 'adic:';

  //TAC ou seu Equiparado, que efetuar o transporte rodoviário de cargas por conta de terceiros e
  //mediante remuneração, indicado no cadastramento da Operação de Transporte.
  //Para o TipoViagem Frota o Contratado será a própria empresa que está declarando a operação.

  with CIOT.AdicionarOperacao.Contratado do
  begin
    if CpfOuCnpj <> '' then
    begin
      Gerador.wGrupoNFSe('Contratado', 'AP82');
      Gerador.wCampoNFSe(tcStr, 'AP83', 'CpfOuCnpj', 01, 01, 1, CpfOuCnpj);
      Gerador.wCampoNFSe(tcStr, 'AP84', 'RNTRC    ', 01, 01, 1, RNTRC);
      Gerador.wGrupoNFSe('/Contratado');
    end;
  end;

  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarMotorista;
var
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := 'adic:';

  //É o condutor do veículo que irá realizar a operação de transporte,
  //pode ser o proprietário do veículo ou não.
  with CIOT.AdicionarOperacao.Motorista do
  begin
    if CpfOuCnpj <> '' then
    begin
      Gerador.wGrupoNFSe('Motorista', 'AP85');
      Gerador.wCampoNFSe(tcStr, 'AP86', 'CpfOuCnpj', 01, 11, 1, CpfOuCnpj, 'CPF ou CNPJ do Motorista.');
      Gerador.wCampoNFSe(tcStr, 'AP87', 'CNH      ', 01, 11, 1, CIOT.AdicionarOperacao.Motorista.CNH);

      Gerador.Prefixo := 'obj:';

      if Celular.Numero <> 0 then
      begin
        Gerador.wGrupoNFSe('Celular', 'AP88');
        Gerador.Prefixo := 'obj1:';
        Gerador.wCampoNFSe(tcInt, 'AP89', 'DDD   ', 01, 02, 1, Celular.DDD, '');
        Gerador.wCampoNFSe(tcInt, 'AP90', 'Numero', 08, 09, 1, Celular.Numero, '');
        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('/Celular');
      end;

      Gerador.Prefixo := 'adic:';
      Gerador.wGrupoNFSe('/Motorista');
    end;
  end;

  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarDestinatario;
begin
  //Destinatário da carga.
  //Na emissão com TipoViagem Padrão seu preenchimento é obrigatório.
  //Na emissão com TipoViagem TAC_Agregado o campo não deve ser preenchido.
  //Não esperado para TipoViagem Frota.
  with CIOT.AdicionarOperacao.Destinatario do
  begin
    if Length(Trim(CpfOuCnpj)) > 0 then
    begin
      Gerador.wGrupoNFSe('Destinatario', 'AP91');
      Gerador.wCampoNFSe(tcStr, 'AP92', 'NomeOuRazaoSocial', 01, 01, 0, NomeOuRazaoSocial);
      Gerador.wCampoNFSe(tcStr, 'AP93', 'CpfOuCnpj        ', 11, 14, 1, CpfOuCnpj);

      with Endereco do
      begin
        if CodigoMunicipio > 0 then
        begin
          Gerador.wGrupoNFSe('Endereco', 'AP94');
          Gerador.Prefixo := 'obj1:';
          Gerador.wCampoNFSe(tcStr, 'AP095', 'Bairro         ', 01, 01, 0, Bairro);
          Gerador.wCampoNFSe(tcStr, 'AP096', 'Rua            ', 01, 01, 0, Rua);
          Gerador.wCampoNFSe(tcStr, 'AP097', 'Numero         ', 01, 01, 0, Numero);
          Gerador.wCampoNFSe(tcStr, 'AP098', 'Complemento    ', 01, 01, 0, Complemento);
          Gerador.wCampoNFSe(tcStr, 'AP099', 'CEP            ', 08, 08, 0, CEP);
          Gerador.wCampoNFSe(tcInt, 'AP100', 'CodigoMunicipio', 07, 07, 1, CodigoMunicipio);
          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Endereco');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP101', 'EMail', 01, 01, 0, EMail);

      with Telefones do
      begin
        if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
        begin
          Gerador.wGrupoNFSe('Telefones', 'AP102');

          Gerador.Prefixo := 'obj1:';
          if Celular.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Celular', 'AP103');
            Gerador.wCampoNFSe(tcInt, 'AP104', 'DDD   ', 01, 02, 1, Celular.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP105', 'Numero', 08, 09, 1, Celular.Numero, '');
            Gerador.wGrupoNFSe('/Celular');
          end;

          if Fixo.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fixo', 'AP106');
            Gerador.wCampoNFSe(tcInt, 'AP107', 'DDD   ', 01, 02, 1, Fixo.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP108', 'Numero', 08, 09, 1, Fixo.Numero, '');
            Gerador.wGrupoNFSe('/Fixo');
          end;

          if Fax.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fax', 'AP109');
            Gerador.wCampoNFSe(tcInt, 'AP110', 'DDD   ', 01, 02, 1, Fax.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP111', 'Numero', 08, 09, 1, Fax.Numero, '');
            Gerador.wGrupoNFSe('/Fax');
          end;

          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Telefones');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP112', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(ResponsavelPeloPagamento, True)), 'Informar se é o responsável pelo pagamento da Operação de Transporte. True = Sim. False = Não');

      Gerador.wGrupoNFSe('/Destinatario');
    end;
  end;
end;

procedure TCIOTW_eFrete.GerarContratante;
begin
  with CIOT.AdicionarOperacao.Contratante do
  begin
    if Length(Trim(CpfOuCnpj)) > 0 then
    begin
      Gerador.wGrupoNFSe('Contratante', 'AP113');
      Gerador.wCampoNFSe(tcStr, 'AP114', 'NomeOuRazaoSocial', 01, 01, 0, NomeOuRazaoSocial);
      Gerador.wCampoNFSe(tcStr, 'AP115', 'CpfOuCnpj        ', 11, 14, 1, CpfOuCnpj);

      with Endereco do
      begin
        if CodigoMunicipio > 0 then
        begin
          Gerador.wGrupoNFSe('Endereco', 'AP116');
          Gerador.Prefixo := 'obj1:';
          Gerador.wCampoNFSe(tcStr, 'AP117', 'Bairro         ', 01, 01, 0, Bairro);
          Gerador.wCampoNFSe(tcStr, 'AP118', 'Rua            ', 01, 01, 0, Rua);
          Gerador.wCampoNFSe(tcStr, 'AP119', 'Numero         ', 01, 01, 0, Numero);
          Gerador.wCampoNFSe(tcStr, 'AP120', 'Complemento    ', 01, 01, 0, Complemento);
          Gerador.wCampoNFSe(tcStr, 'AP121', 'CEP            ', 01, 09, 0, CEP);
          Gerador.wCampoNFSe(tcInt, 'AP122', 'CodigoMunicipio', 07, 07, 1, CodigoMunicipio);
          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Endereco');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP123', 'EMail', 01, 01, 0, EMail);

      with Telefones do
      begin
        if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
        begin
          Gerador.wGrupoNFSe('Telefones', 'AP124');

          Gerador.Prefixo := 'obj1:';

          if Celular.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Celular', 'AP125');
            Gerador.wCampoNFSe(tcInt, 'AP126', 'DDD   ', 01, 02, 0, Celular.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP127', 'Numero', 08, 09, 0, Celular.Numero, '');
            Gerador.wGrupoNFSe('/Celular');
          end;

          if Fixo.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fixo', 'AP128');
            Gerador.wCampoNFSe(tcInt, 'AP129', 'DDD   ', 01, 02, 0, Fixo.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP130', 'Numero', 08, 09, 0, Fixo.Numero, '');
            Gerador.wGrupoNFSe('/Fixo');
          end;

          if Fax.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fax', 'AP131');
            Gerador.wCampoNFSe(tcInt, 'AP132', 'DDD   ', 01, 02, 0, Fax.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP133', 'Numero', 08, 09, 0, Fax.Numero, '');
            Gerador.wGrupoNFSe('/Fax');
          end;

          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Telefones');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP134', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(CIOT.AdicionarOperacao.Contratante.ResponsavelPeloPagamento, True)));
      Gerador.wCampoNFSe(tcStr, 'AP135', 'RNTRC                   ', 01, 01, 1, RNTRC);

      Gerador.wGrupoNFSe('/Contratante');
    end;
  end;
end;

procedure TCIOTW_eFrete.GerarSubContratante;
begin
  //É o transportador que contratar outro transportador para realização do transporte de
  //cargas para o qual fora anteriormente contratado, indicado no cadastramento da Operação de Transporte.
  //Não esperado para TipoViagem Frota.

  with CIOT.AdicionarOperacao.Subcontratante do
  begin
    if Length(Trim(CpfOuCnpj)) > 0 then
    begin
      Gerador.wGrupoNFSe('Subcontratante', 'AP136');
      Gerador.wCampoNFSe(tcStr, 'AP137', 'NomeOuRazaoSocial', 01, 01, 0, NomeOuRazaoSocial);
      Gerador.wCampoNFSe(tcStr, 'AP138', 'CpfOuCnpj        ', 01, 01, 1, CpfOuCnpj);

      with Endereco do
      begin
        if CodigoMunicipio > 0 then
        begin
          Gerador.wGrupoNFSe('Endereco', 'AP139');
          Gerador.Prefixo := 'obj1:';
          Gerador.wCampoNFSe(tcStr, 'AP140', 'Bairro         ', 01, 01, 0, Bairro);
          Gerador.wCampoNFSe(tcStr, 'AP141', 'Rua            ', 01, 01, 0, Rua);
          Gerador.wCampoNFSe(tcStr, 'AP142', 'Numero         ', 01, 01, 0, Numero);
          Gerador.wCampoNFSe(tcStr, 'AP143', 'Complemento    ', 01, 01, 0, Complemento);
          Gerador.wCampoNFSe(tcStr, 'AP144', 'CEP            ', 01, 09, 0, CEP);
          Gerador.wCampoNFSe(tcInt, 'AP145', 'CodigoMunicipio', 01, 07, 1, CodigoMunicipio);
          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Endereco');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP146', 'EMail', 01, 01, 0, EMail);

      with Telefones do
      begin
        if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
        begin
          Gerador.wGrupoNFSe('Telefones', 'AP147');

          Gerador.Prefixo := 'obj1:';

          if Celular.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Celular', 'AP148');
            Gerador.wCampoNFSe(tcInt, 'AP149', 'DDD   ', 01, 02, 1, Celular.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP150', 'Numero', 08, 09, 1, Celular.Numero, '');
            Gerador.wGrupoNFSe('/Celular');
          end;

          if Fixo.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fixo', 'AP151');
            Gerador.wCampoNFSe(tcInt, 'AP152', 'DDD   ', 01, 02, 1, Fixo.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP153', 'Numero', 08, 09, 1, Fixo.Numero, '');
            Gerador.wGrupoNFSe('/Fixo');
          end;

          if Fax.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fax', 'AP154');
            Gerador.wCampoNFSe(tcInt, 'AP155', 'DDD   ', 01, 02, 1, Fax.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP156', 'Numero', 08, 09, 1, Fax.Numero, '');
            Gerador.wGrupoNFSe('/Fax');
          end;

          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Telefones');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP157', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(ResponsavelPeloPagamento, True)));

      Gerador.wGrupoNFSe('/Subcontratante');
    end;
  end;
end;

procedure TCIOTW_eFrete.GerarConsignatario;
begin
  //Aquele que receberá as mercadorias transportadas em consignação,
  //indicado no cadastramento da Operação de Transporte ou nos respectivos documentos fiscais.
  //Não esperado para TipoViagem Frota.

  with CIOT.AdicionarOperacao.Consignatario do
  begin
    if Length(Trim(CpfOuCnpj)) > 0 then
    begin
      Gerador.wGrupoNFSe('Consignatario', 'AP158');
      Gerador.wCampoNFSe(tcStr, 'AP159', 'NomeOuRazaoSocial', 01, 01, 0, NomeOuRazaoSocial);
      Gerador.wCampoNFSe(tcStr, 'AP160', 'CpfOuCnpj        ', 01, 01, 1, CpfOuCnpj);

      with Endereco do
      begin
        if CodigoMunicipio > 0 then
        begin
          Gerador.wGrupoNFSe('Endereco', 'AP161');
          Gerador.Prefixo := 'obj1:';
          Gerador.wCampoNFSe(tcStr, 'AP162', 'Bairro         ', 01, 01, 0, Bairro);
          Gerador.wCampoNFSe(tcStr, 'AP163', 'Rua            ', 01, 01, 0, Rua);
          Gerador.wCampoNFSe(tcStr, 'AP164', 'Numero         ', 01, 01, 0, Numero);
          Gerador.wCampoNFSe(tcStr, 'AP165', 'Complemento    ', 01, 01, 0, Complemento);
          Gerador.wCampoNFSe(tcStr, 'AP166', 'CEP            ', 01, 09, 0, CEP);
          Gerador.wCampoNFSe(tcInt, 'AP167', 'CodigoMunicipio', 01, 07, 1, CodigoMunicipio);
          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Endereco');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP168', 'EMail', 01, 01, 0, EMail);

      with Telefones do
      begin
        if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
        begin
          Gerador.wGrupoNFSe('Telefones', 'AP169');

          Gerador.Prefixo := 'obj1:';

          if Celular.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Celular', 'AP170');
            Gerador.wCampoNFSe(tcInt, 'AP171', 'DDD   ', 01, 02, 1, Celular.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP172', 'Numero', 08, 09, 1, Celular.Numero, '');
            Gerador.wGrupoNFSe('/Celular');
          end;

          if Fixo.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fixo', 'AP173');
            Gerador.wCampoNFSe(tcInt, 'AP174', 'DDD   ', 01, 02, 1, Fixo.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP175', 'Numero', 08, 09, 1, Fixo.Numero, '');
            Gerador.wGrupoNFSe('/Fixo');
          end;

          if Fax.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fax', 'AP176');
            Gerador.wCampoNFSe(tcInt, 'AP177', 'DDD   ', 01, 02, 1, Fax.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP178', 'Numero', 08, 09, 1, Fax.Numero, '');
            Gerador.wGrupoNFSe('/Fax');
          end;

          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Telefones');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP179', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(ResponsavelPeloPagamento, True)));

      Gerador.wGrupoNFSe('/Consignatario');
    end;
  end;
end;

procedure TCIOTW_eFrete.GerarTomadorServico;
begin
  //Pessoa (física ou jurídica) que contratou o frete pela transportadora.
  //Na emissão com TipoViagem Padrão seu preenchimento é obrigatório.
  //Na emissão com TipoViagem TAC_Agregado o campo não deve ser preenchido.

  with CIOT.AdicionarOperacao.TomadorServico do
  begin
    if Length(Trim(CpfOuCnpj)) > 0 then
    begin
      Gerador.wGrupoNFSe('TomadorServico', 'AP180');
      Gerador.wCampoNFSe(tcStr, 'AP181', 'NomeOuRazaoSocial', 01, 01, 0, NomeOuRazaoSocial);
      Gerador.wCampoNFSe(tcStr, 'AP182', 'CpfOuCnpj        ', 01, 01, 1, CpfOuCnpj);

      with Endereco do
      begin
        if CodigoMunicipio > 0 then
        begin
          Gerador.wGrupoNFSe('Endereco', 'AP183');
          Gerador.Prefixo := 'obj1:';
          Gerador.wCampoNFSe(tcStr, 'AP184', 'Bairro         ', 01, 01, 0, Bairro);
          Gerador.wCampoNFSe(tcStr, 'AP185', 'Rua            ', 01, 01, 0, Rua);
          Gerador.wCampoNFSe(tcStr, 'AP186', 'Numero         ', 01, 01, 0, Numero);
          Gerador.wCampoNFSe(tcStr, 'AP187', 'Complemento    ', 01, 01, 0, Complemento);
          Gerador.wCampoNFSe(tcStr, 'AP188', 'CEP            ', 01, 09, 0, CEP);
          Gerador.wCampoNFSe(tcInt, 'AP189', 'CodigoMunicipio', 01, 07, 1, CodigoMunicipio);
          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Endereco');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP190', 'EMail', 01, 01, 0, EMail);

      with Telefones do
      begin
        if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
        begin
          Gerador.wGrupoNFSe('Telefones', 'AP191');

          Gerador.Prefixo := 'obj1:';

          if Celular.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Celular', 'AP192');
            Gerador.wCampoNFSe(tcInt, 'AP193', 'DDD   ', 01, 02, 1, Celular.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP194', 'Numero', 08, 09, 1, Celular.Numero, '');
            Gerador.wGrupoNFSe('/Celular');
          end;

          if Fixo.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fixo', 'AP195');
            Gerador.wCampoNFSe(tcInt, 'AP196', 'DDD   ', 01, 02, 1, Fixo.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP197', 'Numero', 08, 09, 1, Fixo.Numero, '');
            Gerador.wGrupoNFSe('/Fixo');
          end;

          if Fax.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fax', 'AP198');
            Gerador.wCampoNFSe(tcInt, 'AP199', 'DDD   ', 01, 02, 1, Fax.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP200', 'Numero', 08, 09, 1, Fax.Numero, '');
            Gerador.wGrupoNFSe('/Fax');
          end;

          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Telefones');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP201', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(ResponsavelPeloPagamento, True)));

      Gerador.wGrupoNFSe('/TomadorServico');
    end;
  end;
end;

procedure TCIOTW_eFrete.GerarRemetente;
begin
  with CIOT.AdicionarOperacao.Remetente do
  begin
    if Length(Trim(CpfOuCnpj)) > 0 then
    begin
      Gerador.wGrupoNFSe('Remetente', 'AP202');
      Gerador.wCampoNFSe(tcStr, 'AP203', 'NomeOuRazaoSocial', 01, 01, 0, NomeOuRazaoSocial);
      Gerador.wCampoNFSe(tcStr, 'AP204', 'CpfOuCnpj        ', 01, 01, 1, CpfOuCnpj);

      with Endereco do
      begin
        if CodigoMunicipio > 0 then
        begin
          Gerador.wGrupoNFSe('Endereco', 'AP205');
          Gerador.Prefixo := 'obj1:';
          Gerador.wCampoNFSe(tcStr, 'AP206', 'Bairro         ', 01, 01, 0, Bairro);
          Gerador.wCampoNFSe(tcStr, 'AP207', 'Rua            ', 01, 01, 0, Rua);
          Gerador.wCampoNFSe(tcStr, 'AP208', 'Numero         ', 01, 01, 0, Numero);
          Gerador.wCampoNFSe(tcStr, 'AP209', 'Complemento    ', 01, 01, 0, Complemento);
          Gerador.wCampoNFSe(tcStr, 'AP210', 'CEP            ', 01, 09, 0, CEP);
          Gerador.wCampoNFSe(tcInt, 'AP211', 'CodigoMunicipio', 01, 07, 1, CodigoMunicipio);
          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Endereco');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP212', 'EMail', 01, 01, 0, EMail);

      with Telefones do
      begin
        if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
        begin
          Gerador.wGrupoNFSe('Telefones', 'AP213');

          Gerador.Prefixo := 'obj1:';

          if Celular.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Celular', 'AP214');
            Gerador.wCampoNFSe(tcInt, 'AP215', 'DDD   ', 01, 02, 1, Celular.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP216', 'Numero', 08, 09, 1, Celular.Numero, '');
            Gerador.wGrupoNFSe('/Celular');
          end;

          if Fixo.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fixo', 'AP217');
            Gerador.wCampoNFSe(tcInt, 'AP218', 'DDD   ', 01, 02, 1, Fixo.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP219', 'Numero', 08, 09, 1, Fixo.Numero, '');
            Gerador.wGrupoNFSe('/Fixo');
          end;

          if Fax.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fax', 'AP220');
            Gerador.wCampoNFSe(tcInt, 'AP221', 'DDD   ', 01, 02, 1, Fax.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP222', 'Numero', 08, 09, 1, Fax.Numero, '');
            Gerador.wGrupoNFSe('/Fax');
          end;

          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Telefones');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP223', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(ResponsavelPeloPagamento, True)));

      Gerador.wGrupoNFSe('/Remetente');
    end;
  end;
end;

procedure TCIOTW_eFrete.GerarProprietarioCarga;
begin
  with CIOT.AdicionarOperacao.ProprietarioCarga do
  begin
    if Length(Trim(CpfOuCnpj)) > 0 then
    begin
      Gerador.wGrupoNFSe('ProprietarioCarga', 'AP224');
      Gerador.wCampoNFSe(tcStr, 'AP225', 'NomeOuRazaoSocial', 01, 01, 0, NomeOuRazaoSocial);
      Gerador.wCampoNFSe(tcStr, 'AP226', 'CpfOuCnpj        ', 01, 01, 1, CpfOuCnpj);

      with Endereco do
      begin
        if CodigoMunicipio > 0 then
        begin
          Gerador.wGrupoNFSe('Endereco', 'AP227');
          Gerador.Prefixo := 'obj1:';
          Gerador.wCampoNFSe(tcStr, 'AP228', 'Bairro         ', 01, 01, 0, Bairro);
          Gerador.wCampoNFSe(tcStr, 'AP229', 'Rua            ', 01, 01, 0, Rua);
          Gerador.wCampoNFSe(tcStr, 'AP230', 'Numero         ', 01, 01, 0, Numero);
          Gerador.wCampoNFSe(tcStr, 'AP231', 'Complemento    ', 01, 01, 0, Complemento);
          Gerador.wCampoNFSe(tcStr, 'AP232', 'CEP            ', 01, 09, 0, CEP);
          Gerador.wCampoNFSe(tcInt, 'AP233', 'CodigoMunicipio', 01, 07, 1, CodigoMunicipio);
          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Endereco');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP234', 'EMail', 01, 01, 0, EMail);

      with Telefones do
      begin
        if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
        begin
          Gerador.wGrupoNFSe('Telefones', 'AP235');

          Gerador.Prefixo := 'obj1:';

          if Celular.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Celular', 'AP236');
            Gerador.wCampoNFSe(tcInt, 'AP237', 'DDD   ', 01, 02, 1, Celular.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP238', 'Numero', 08, 09, 1, Celular.Numero, '');
            Gerador.wGrupoNFSe('/Celular');
          end;

          if Fixo.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fixo', 'AP239');
            Gerador.wCampoNFSe(tcInt, 'AP240', 'DDD   ', 01, 02, 1, Fixo.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP241', 'Numero', 08, 09, 1, Fixo.Numero, '');
            Gerador.wGrupoNFSe('/Fixo');
          end;

          if Fax.Numero > 0 then
          begin
            Gerador.wGrupoNFSe('Fax', 'AP242');
            Gerador.wCampoNFSe(tcInt, 'AP243', 'DDD   ', 01, 02, 1, Fax.DDD, '');
            Gerador.wCampoNFSe(tcInt, 'AP244', 'Numero', 08, 09, 1, Fax.Numero, '');
            Gerador.wGrupoNFSe('/Fax');
          end;

          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('/Telefones');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP245', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(ResponsavelPeloPagamento, True)));

      Gerador.wGrupoNFSe('/ProprietarioCarga');
    end;
  end;
end;

procedure TCIOTW_eFrete.GerarVeiculos(xPrefixo: String);
var
  i: Integer;
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := xPrefixo;

  if xPrefixo = 'adic:' then
  begin
    for i := 0 to CIOT.AdicionarOperacao.Veiculos.Count -1 do
    begin
      Gerador.wGrupoNFSe('Veiculos', 'AP246');
      Gerador.wCampoNFSe(tcStr, 'AP247', 'Placa', 01, 07, 1, CIOT.AdicionarOperacao.Veiculos.Items[I].Placa);
      Gerador.wGrupoNFSe('/Veiculos');
    end;
  end;

  if xPrefixo = 'ret:' then
  begin
    if CIOT.RetificarOperacao.Veiculos.Count > 0 then
    begin
      Gerador.wGrupoNFSe('Veiculos', 'AP201');

      for i := 0 to CIOT.RetificarOperacao.Veiculos.Count -1 do
      begin
        Gerador.wGrupoNFSe('Veiculo', 'AP201');
        Gerador.wCampoNFSe(tcStr, 'AP202', 'Placa', 01, 07, 1, CIOT.RetificarOperacao.Veiculos.Items[I].Placa);
        Gerador.wGrupoNFSe('/Veiculo');
      end;

      Gerador.wGrupoNFSe('/Veiculos');
    end;
  end;

  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarViagemAdicViagem;
var
  i, j: Integer;
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := 'adic:';

  Gerador.wGrupoNFSe('Viagens', 'AP12');

  for I := 0 to CIOT.AdicionarViagem.Viagens.Count -1 do
  begin
    Gerador.wGrupoNFSe('Viagem', 'AP12');

    with CIOT.AdicionarViagem.Viagens.Items[I] do
    begin
      Gerador.wCampoNFSe(tcStr, 'AP13', 'DocumentoViagem       ', 01, 01, 0, DocumentoViagem);
      Gerador.wCampoNFSe(tcInt, 'AP14', 'CodigoMunicipioOrigem ', 01, 07, 1, CodigoMunicipioOrigem);
      Gerador.wCampoNFSe(tcInt, 'AP15', 'CodigoMunicipioDestino', 01, 07, 1, CodigoMunicipioDestino);
      Gerador.wCampoNFSe(tcStr, 'AP16', 'CepOrigem             ', 01, 01, 0, CepOrigem);
      Gerador.wCampoNFSe(tcStr, 'AP17', 'CepDestino            ', 01, 01, 0, CepDestino);

      Gerador.Prefixo := 'obj:';

      if Valores.TotalOperacao > 0 then
      begin
        Gerador.wGrupoNFSe('Valores', 'AP19');

        with Valores do
        begin
          Gerador.wCampoNFSe(tcDe2, 'AP20', 'TotalOperacao              ', 01, 01, 1, TotalOperacao);
          Gerador.wCampoNFSe(tcDe2, 'AP21', 'TotalViagem                ', 01, 01, 1, TotalViagem);
          Gerador.wCampoNFSe(tcDe2, 'AP22', 'TotalDeAdiantamento        ', 01, 01, 1, TotalDeAdiantamento);
          Gerador.wCampoNFSe(tcDe2, 'AP23', 'TotalDeQuitacao            ', 01, 01, 1, TotalDeQuitacao);
          Gerador.wCampoNFSe(tcDe2, 'AP24', 'Combustivel                ', 01, 01, 1, Combustivel);
          Gerador.wCampoNFSe(tcDe2, 'AP25', 'Pedagio                    ', 01, 01, 1, Pedagio);
          Gerador.wCampoNFSe(tcDe2, 'AP26', 'OutrosCreditos             ', 01, 01, 1, OutrosCreditos);
          Gerador.wCampoNFSe(tcStr, 'AP27', 'JustificativaOutrosCreditos', 01, 01, 0, JustificativaOutrosCreditos);
          Gerador.wCampoNFSe(tcDe2, 'AP28', 'Seguro                     ', 01, 01, 1, Seguro);
          Gerador.wCampoNFSe(tcDe2, 'AP29', 'OutrosDebitos              ', 01, 01, 1, OutrosDebitos);
          Gerador.wCampoNFSe(tcStr, 'AP30', 'JustificativaOutrosDebitos ', 01, 01, 0, JustificativaOutrosDebitos);
        end;

        Gerador.wGrupoNFSe('/Valores');
      end;

      Gerador.wCampoNFSe(tcStr, 'AP31', 'TipoPagamento', 001, 020, 1, TpPagamentoToStr(TipoPagamento));

      with InformacoesBancarias do
      begin
        if (InstituicaoBancaria <> '') or (Agencia <> '') or (Conta <> '') then
        begin
          Gerador.wGrupoNFSe('InformacoesBancarias', 'AP32');

          Gerador.wCampoNFSe(tcStr, 'AP33', 'InstituicaoBancaria', 01, 01, 0, InstituicaoBancaria);
          Gerador.wCampoNFSe(tcStr, 'AP34', 'Agencia            ', 01, 01, 0, Agencia);
          Gerador.wCampoNFSe(tcStr, 'AP35', 'Conta              ', 01, 01, 0, Conta);
          Gerador.wCampoNFSe(tcStr, 'AP36', 'TipoConta          ', 01, 15, 1, TipoContaToStr(TipoConta));

          Gerador.wGrupoNFSe('/InformacoesBancarias');
        end;
      end;

      if NotasFiscais.Count > 0 then
      begin
        Gerador.Prefixo := 'adic:';

        for J := 0 to NotasFiscais.Count -1 do
        begin
          with NotasFiscais.Items[J] do
          begin
            Gerador.wGrupoNFSe('NotasFiscais', 'AP38');
            Gerador.wCampoNFSe(tcStr, 'AP39', 'Numero                            ', 01, 01, 0, Numero);
            Gerador.wCampoNFSe(tcStr, 'AP40', 'Serie                             ', 01, 01, 0, Serie);
            Gerador.wCampoNFSe(tcDat, 'AP41', 'Data                              ', 01, 01, 1, Data);
            Gerador.wCampoNFSe(tcDe2, 'AP42', 'ValorTotal                        ', 01, 01, 1, ValorTotal);
            Gerador.wCampoNFSe(tcDe4, 'AP43', 'ValorDaMercadoriaPorUnidade       ', 01, 01, 1, ValorDaMercadoriaPorUnidade);
            Gerador.wCampoNFSe(tcInt, 'AP44', 'CodigoNCMNaturezaCarga            ', 01, 04, 1, CodigoNCMNaturezaCarga);
            Gerador.wCampoNFSe(tcStr, 'AP45', 'DescricaoDaMercadoria             ', 01, 01, 0, DescricaoDaMercadoria);
            Gerador.wCampoNFSe(tcStr, 'AP46', 'UnidadeDeMedidaDaMercadoria       ', 01, 01, 1, TpUnMedMercToStr(UnidadeDeMedidaDaMercadoria));
            Gerador.wCampoNFSe(tcStr, 'AP47', 'TipoDeCalculo                     ', 01, 01, 1, TpVgTipoCalculoToStr(TipoDeCalculo));
            Gerador.wCampoNFSe(tcDe4, 'AP48', 'ValorDoFretePorUnidadeDeMercadoria', 01, 01, 1, ValorDoFretePorUnidadeDeMercadoria);
            Gerador.wCampoNFSe(tcDe4, 'AP49', 'QuantidadeDaMercadoriaNoEmbarque  ', 01, 01, 1, QuantidadeDaMercadoriaNoEmbarque);

            if ToleranciaDePerdaDeMercadoria.Valor > 0 then
            begin
              Gerador.wGrupoNFSe('ToleranciaDePerdaDeMercadoria', 'AP50');
              Gerador.wCampoNFSe(tcStr, 'AP51', 'Tipo ', 01, 01, 1, TpProporcaoToStr(ToleranciaDePerdaDeMercadoria.Tipo));
              Gerador.wCampoNFSe(tcDe2, 'AP52', 'Valor', 01, 01, 1, ToleranciaDePerdaDeMercadoria.Valor);
              Gerador.wGrupoNFSe('/ToleranciaDePerdaDeMercadoria');
            end;

            Gerador.wGrupoNFSe('/NotasFiscais');
          end;
        end;
      end;
    end;

    Gerador.wGrupoNFSe('/Viagem');
  end;

  Gerador.wGrupoNFSe('/Viagens');

  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarPagamentosAdicViagem;
var
  i: Integer;
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := 'adic:';

  //Pagamentos registrados.
  //- Pode existir mais de 1 pagamento com uma mesma categoria (exceto para Quitacao).
  //- A soma dos pagamentos c/ categoria Adiantamento, deverá ter o mesmo valor apontado na
  //     tag TotalAdiantamento da tag Viagem/Valores, e neste caso, a tag Documento do pagamento
  //     deverá conter o mesmo valor da tag DocumentoViagem da tag Viagem .
  //- Se a viagem possuir a tag TotalQuitacao maior que zero, deverá ter um pagamento correspondente,
  //     com Categoria Quitacao e com o Documento o mesmo valor apontado na tag DocumentoViagem .
  Gerador.wGrupoNFSe('Pagamentos', 'AP68');

  for i := 0 to CIOT.AdicionarOperacao.Pagamentos.Count -1 do
  begin
    with CIOT.AdicionarOperacao.Pagamentos.Items[i] do
    begin
      Gerador.wGrupoNFSe('Pagamento', 'AP68');
      Gerador.wCampoNFSe(tcStr, 'AP69', 'IdPagamentoCliente', 01, 01, 0, IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente.');
      Gerador.wCampoNFSe(tcDat, 'AP70', 'DataDeLiberacao   ', 01, 01, 1, DataDeLiberacao, 'Data em que o pagamento será liberado para saque.');
      Gerador.wCampoNFSe(tcDe2, 'AP71', 'Valor             ', 01, 01, 1, Valor, 'Valor do pagamento.');

      Gerador.Prefixo := 'obj:';
      Gerador.wCampoNFSe(tcStr, 'AP72', 'TipoPagamento', 01, 01, 1, TpPagamentoToStr(TipoPagamento), 'Tipo de pagamento que será usado pelo contratante. Restrito aos itens da enum: -TransferenciaBancaria -eFRETE');
      Gerador.wCampoNFSe(tcStr, 'AP73', 'Categoria    ', 01, 01, 1, TpCatPagToStr(Categoria), 'Categoria relacionada ao pagamento realizado. Restrita aos membros da ENUM: -Adiantamento, -Estadia, -Quitacao, -SemCategoria, -Frota ');

      Gerador.Prefixo := 'adic:';
      Gerador.wCampoNFSe(tcStr, 'AP74', 'Documento', 01, 01, 0, Documento, 'Documento relacionado a viagem.');

      // Preenchimento obrigatório para o TipoPagamento TransferenciaBancaria.
      // Não deve ser preenchido para TipoPagamento eFRETE.
      with InformacoesBancarias do
      begin
        if (InstituicaoBancaria <> '') or (Agencia <> '') or (Conta <> '') then
        begin
          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('InformacoesBancarias', 'AP75');

          Gerador.wCampoNFSe(tcStr, 'AP76', 'InstituicaoBancaria', 01, 01, 0, InstituicaoBancaria, 'Código de compensação da instituição bancária que será realizado o pagamento. ');
          Gerador.wCampoNFSe(tcStr, 'AP77', 'Agencia            ', 01, 01, 0, Agencia, 'Agência na qual o contratado possui conta com dígito (se houver).');
          Gerador.wCampoNFSe(tcStr, 'AP78', 'Conta              ', 01, 01, 0, Conta, 'Conta do contratado com dígito. ');
          Gerador.wCampoNFSe(tcStr, 'AP79', 'TipoConta          ', 01, 15, 1, TipoContaToStr(TipoConta));

          Gerador.wGrupoNFSe('/InformacoesBancarias');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP80', 'InformacaoAdicional', 01, 01, 0, InformacaoAdicional);

      if Categoria = tcpFrota then
        Gerador.wCampoNFSe(tcStr, 'AP81', 'CnpjFilialAbastecimento', 01, 01, 1, CnpjFilialAbastecimento);

      Gerador.Prefixo := 'adic:';
      Gerador.wGrupoNFSe('/Pagamento');
    end;
  end;

  Gerador.wGrupoNFSe('/Pagamentos');
  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarPagamentosAdicPagamento;
var
  i: Integer;
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := 'adic:';

  //Pagamentos registrados.
  //- Pode existir mais de 1 pagamento com uma mesma categoria (exceto para Quitacao).
  //- A soma dos pagamentos c/ categoria Adiantamento, deverá ter o mesmo valor apontado na
  //     tag TotalAdiantamento da tag Viagem/Valores, e neste caso, a tag Documento do pagamento
  //     deverá conter o mesmo valor da tag DocumentoViagem da tag Viagem .
  //- Se a viagem possuir a tag TotalQuitacao maior que zero, deverá ter um pagamento correspondente,
  //     com Categoria Quitacao e com o Documento o mesmo valor apontado na tag DocumentoViagem .
  Gerador.wGrupoNFSe('Pagamentos', 'AP68');

  for i := 0 to CIOT.AdicionarOperacao.Pagamentos.Count -1 do
  begin
    with CIOT.AdicionarOperacao.Pagamentos.Items[i] do
    begin
      Gerador.wGrupoNFSe('Pagamento', 'AP68');
      Gerador.wCampoNFSe(tcStr, 'AP69', 'IdPagamentoCliente', 01, 01, 0, IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente.');
      Gerador.wCampoNFSe(tcDat, 'AP70', 'DataDeLiberacao   ', 01, 01, 1, DataDeLiberacao, 'Data em que o pagamento será liberado para saque.');
      Gerador.wCampoNFSe(tcDe2, 'AP71', 'Valor             ', 01, 01, 1, Valor, 'Valor do pagamento.');

      Gerador.Prefixo := 'obj:';
      Gerador.wCampoNFSe(tcStr, 'AP72', 'TipoPagamento', 01, 01, 1, TpPagamentoToStr(TipoPagamento), 'Tipo de pagamento que será usado pelo contratante. Restrito aos itens da enum: -TransferenciaBancaria -eFRETE');
      Gerador.wCampoNFSe(tcStr, 'AP73', 'Categoria    ', 01, 01, 1, TpCatPagToStr(Categoria), 'Categoria relacionada ao pagamento realizado. Restrita aos membros da ENUM: -Adiantamento, -Estadia, -Quitacao, -SemCategoria, -Frota ');

      Gerador.Prefixo := 'adic:';
      Gerador.wCampoNFSe(tcStr, 'AP74', 'Documento', 01, 01, 0, Documento, 'Documento relacionado a viagem.');

      // Preenchimento obrigatório para o TipoPagamento TransferenciaBancaria.
      // Não deve ser preenchido para TipoPagamento eFRETE.
      with InformacoesBancarias do
      begin
        if (InstituicaoBancaria <> '') or (Agencia <> '') or (Conta <> '') then
        begin
          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('InformacoesBancarias', 'AP75');

          Gerador.wCampoNFSe(tcStr, 'AP76', 'InstituicaoBancaria', 01, 01, 0, InstituicaoBancaria, 'Código de compensação da instituição bancária que será realizado o pagamento. ');
          Gerador.wCampoNFSe(tcStr, 'AP77', 'Agencia            ', 01, 01, 0, Agencia, 'Agência na qual o contratado possui conta com dígito (se houver).');
          Gerador.wCampoNFSe(tcStr, 'AP78', 'Conta              ', 01, 01, 0, Conta, 'Conta do contratado com dígito. ');
          Gerador.wCampoNFSe(tcStr, 'AP79', 'TipoConta          ', 01, 15, 1, TipoContaToStr(TipoConta));

          Gerador.wGrupoNFSe('/InformacoesBancarias');
        end;
      end;

      Gerador.Prefixo := 'adic:';
      Gerador.wCampoNFSe(tcStr, 'AP80', 'InformacaoAdicional', 01, 01, 0, InformacaoAdicional);

      if Categoria = tcpFrota then
        Gerador.wCampoNFSe(tcStr, 'AP81', 'CnpjFilialAbastecimento', 01, 01, 1, CnpjFilialAbastecimento);

      Gerador.wGrupoNFSe('/Pagamento');
    end;
  end;

  Gerador.wGrupoNFSe('/Pagamentos');
  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarViagemEncerramento;
var
  i, j: Integer;
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := 'enc:';

  Gerador.wGrupoNFSe('Viagens', 'AP12');

  for I := 0 to CIOT.EncerrarOperacao.Viagens.Count -1 do
  begin
    Gerador.wGrupoNFSe('Viagem', 'AP12');

    with CIOT.EncerrarOperacao.Viagens.Items[I] do
    begin
      Gerador.wCampoNFSe(tcStr, 'AP13', 'DocumentoViagem       ', 01, 01, 0, DocumentoViagem);
      Gerador.wCampoNFSe(tcInt, 'AP14', 'CodigoMunicipioOrigem ', 01, 07, 1, CodigoMunicipioOrigem);
      Gerador.wCampoNFSe(tcInt, 'AP15', 'CodigoMunicipioDestino', 01, 07, 1, CodigoMunicipioDestino);
      Gerador.wCampoNFSe(tcStr, 'AP16', 'CepOrigem             ', 01, 01, 0, CepOrigem);
      Gerador.wCampoNFSe(tcStr, 'AP17', 'CepDestino            ', 01, 01, 0, CepDestino);

      Gerador.Prefixo := 'obj:';

      if Valores.TotalOperacao > 0 then
      begin
        Gerador.wGrupoNFSe('Valores', 'AP19');

        with Valores do
        begin
          Gerador.wCampoNFSe(tcDe2, 'AP20', 'TotalOperacao              ', 01, 01, 1, TotalOperacao);
          Gerador.wCampoNFSe(tcDe2, 'AP21', 'TotalViagem                ', 01, 01, 1, TotalViagem);
          Gerador.wCampoNFSe(tcDe2, 'AP22', 'TotalDeAdiantamento        ', 01, 01, 1, TotalDeAdiantamento);
          Gerador.wCampoNFSe(tcDe2, 'AP23', 'TotalDeQuitacao            ', 01, 01, 1, TotalDeQuitacao);
          Gerador.wCampoNFSe(tcDe2, 'AP24', 'Combustivel                ', 01, 01, 1, Combustivel);
          Gerador.wCampoNFSe(tcDe2, 'AP25', 'Pedagio                    ', 01, 01, 1, Pedagio);
          Gerador.wCampoNFSe(tcDe2, 'AP26', 'OutrosCreditos             ', 01, 01, 1, OutrosCreditos);
          Gerador.wCampoNFSe(tcStr, 'AP27', 'JustificativaOutrosCreditos', 01, 01, 0, JustificativaOutrosCreditos);
          Gerador.wCampoNFSe(tcDe2, 'AP28', 'Seguro                     ', 01, 01, 1, Seguro);
          Gerador.wCampoNFSe(tcDe2, 'AP29', 'OutrosDebitos              ', 01, 01, 1, OutrosDebitos);
          Gerador.wCampoNFSe(tcStr, 'AP30', 'JustificativaOutrosDebitos ', 01, 01, 0, JustificativaOutrosDebitos);
        end;

        Gerador.wGrupoNFSe('/Valores');
      end;

      Gerador.wCampoNFSe(tcStr, 'AP31', 'TipoPagamento', 001, 020, 1, TpPagamentoToStr(TipoPagamento));

      with InformacoesBancarias do
      begin
        if (InstituicaoBancaria <> '') or (Agencia <> '') or (Conta <> '') then
        begin
          Gerador.wGrupoNFSe('InformacoesBancarias', 'AP32');

          Gerador.wCampoNFSe(tcStr, 'AP33', 'InstituicaoBancaria', 01, 01, 0, InstituicaoBancaria);
          Gerador.wCampoNFSe(tcStr, 'AP34', 'Agencia            ', 01, 01, 0, Agencia);
          Gerador.wCampoNFSe(tcStr, 'AP35', 'Conta              ', 01, 01, 0, Conta);
          Gerador.wCampoNFSe(tcStr, 'AP36', 'TipoConta          ', 01, 15, 1, TipoContaToStr(TipoConta));

          Gerador.wGrupoNFSe('/InformacoesBancarias');
        end;
      end;

      if NotasFiscais.Count > 0 then
      begin
        Gerador.Prefixo := 'enc:';
        Gerador.wGrupoNFSe('NotasFiscais', 'AP37');

        for J := 0 to NotasFiscais.Count -1 do
        begin
          with NotasFiscais.Items[J] do
          begin
            Gerador.wGrupoNFSe('NotaFiscal', 'AP38');
            Gerador.wCampoNFSe(tcStr, 'AP39', 'Numero                             ', 01, 01, 0, Numero);
            Gerador.wCampoNFSe(tcStr, 'AP40', 'Serie                              ', 01, 01, 0, Serie);
            Gerador.wCampoNFSe(tcDat, 'AP41', 'Data                               ', 01, 01, 1, Data);
            Gerador.wCampoNFSe(tcDe2, 'AP42', 'ValorTotal                         ', 01, 01, 1, ValorTotal);
            Gerador.wCampoNFSe(tcDe4, 'AP43', 'ValorDaMercadoriaPorUnidade        ', 01, 01, 1, ValorDaMercadoriaPorUnidade);
            Gerador.wCampoNFSe(tcInt, 'AP44', 'CodigoNCMNaturezaCarga             ', 01, 04, 1, CodigoNCMNaturezaCarga);
            Gerador.wCampoNFSe(tcStr, 'AP45', 'DescricaoDaMercadoria              ', 01, 01, 0, DescricaoDaMercadoria);
            Gerador.wCampoNFSe(tcStr, 'AP46', 'UnidadeDeMedidaDaMercadoria        ', 01, 01, 1, TpUnMedMercToStr(UnidadeDeMedidaDaMercadoria));
            Gerador.wCampoNFSe(tcStr, 'AP47', 'TipoDeCalculo                      ', 01, 01, 1, TpVgTipoCalculoToStr(TipoDeCalculo));
            Gerador.wCampoNFSe(tcDe4, 'AP48', 'ValorDoFretePorUnidadeDeMercadoria ', 01, 01, 1, ValorDoFretePorUnidadeDeMercadoria);
            Gerador.wCampoNFSe(tcDe4, 'AP49', 'QuantidadeDaMercadoriaNoEmbarque   ', 01, 01, 1, QuantidadeDaMercadoriaNoEmbarque);
            Gerador.wCampoNFSe(tcDe4, 'AP49', 'QuantidadeDaMercadoriaNoDesembarque', 01, 01, 1, QuantidadeDaMercadoriaNoDesembarque);

            if ToleranciaDePerdaDeMercadoria.Valor > 0 then
            begin
              Gerador.wGrupoNFSe('ToleranciaDePerdaDeMercadoria', 'AP50');
              Gerador.wCampoNFSe(tcStr, 'AP51', 'Tipo ', 01, 01, 1, TpProporcaoToStr(ToleranciaDePerdaDeMercadoria.Tipo));
              Gerador.wCampoNFSe(tcDe2, 'AP52', 'Valor', 01, 01, 1, ToleranciaDePerdaDeMercadoria.Valor);
              Gerador.wGrupoNFSe('/ToleranciaDePerdaDeMercadoria');
            end;

            Gerador.wGrupoNFSe('/NotaFiscal');
          end;
        end;
        Gerador.wGrupoNFSe('/NotasFiscais');
      end;
    end;

    Gerador.wGrupoNFSe('/Viagem');
  end;

  Gerador.wGrupoNFSe('/Viagens');

  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarPagamentosEncerramento;
var
  i: Integer;
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := 'enc:';

  //Pagamentos registrados.
  //- Pode existir mais de 1 pagamento com uma mesma categoria (exceto para Quitacao).
  //- A soma dos pagamentos c/ categoria Adiantamento, deverá ter o mesmo valor apontado na
  //     tag TotalAdiantamento da tag Viagem/Valores, e neste caso, a tag Documento do pagamento
  //     deverá conter o mesmo valor da tag DocumentoViagem da tag Viagem .
  //- Se a viagem possuir a tag TotalQuitacao maior que zero, deverá ter um pagamento correspondente,
  //     com Categoria Quitacao e com o Documento o mesmo valor apontado na tag DocumentoViagem .
  Gerador.wGrupoNFSe('Pagamentos', 'AP68');

  for i := 0 to CIOT.AdicionarOperacao.Pagamentos.Count -1 do
  begin
    with CIOT.EncerrarOperacao.Pagamentos.Items[i] do
    begin
      Gerador.wGrupoNFSe('Pagamento', 'AP68');
      Gerador.wCampoNFSe(tcStr, 'AP69', 'IdPagamentoCliente', 01, 01, 0, IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente.');
      Gerador.wCampoNFSe(tcDat, 'AP70', 'DataDeLiberacao   ', 01, 01, 1, DataDeLiberacao, 'Data em que o pagamento será liberado para saque.');
      Gerador.wCampoNFSe(tcDe2, 'AP71', 'Valor             ', 01, 01, 1, Valor, 'Valor do pagamento.');
      Gerador.wCampoNFSe(tcStr, 'AP72', 'TipoPagamento     ', 01, 01, 1, TpPagamentoToStr(TipoPagamento), 'Tipo de pagamento que será usado pelo contratante. Restrito aos itens da enum: -TransferenciaBancaria -eFRETE');

      // Preenchimento obrigatório para o TipoPagamento TransferenciaBancaria.
      // Não deve ser preenchido para TipoPagamento eFRETE.
      with InformacoesBancarias do
      begin
        if (InstituicaoBancaria <> '') or (Agencia <> '') or (Conta <> '') then
        begin
          Gerador.wGrupoNFSe('InformacoesBancarias', 'AP75');
          Gerador.Prefixo := 'obj:';

          Gerador.wCampoNFSe(tcStr, 'AP76', 'InstituicaoBancaria', 01, 01, 0, InstituicaoBancaria, 'Código de compensação da instituição bancária que será realizado o pagamento. ');
          Gerador.wCampoNFSe(tcStr, 'AP77', 'Agencia            ', 01, 01, 0, Agencia, 'Agência na qual o contratado possui conta com dígito (se houver).');
          Gerador.wCampoNFSe(tcStr, 'AP78', 'Conta              ', 01, 01, 0, Conta, 'Conta do contratado com dígito. ');
          Gerador.wCampoNFSe(tcStr, 'AP79', 'TipoConta          ', 01, 15, 1, TipoContaToStr(TipoConta));

          Gerador.wGrupoNFSe('/InformacoesBancarias');
        end;
      end;

      Gerador.Prefixo := 'enc:';
      Gerador.wCampoNFSe(tcStr, 'AP80', 'InformacaoAdicional', 01, 01, 0, InformacaoAdicional);

      Gerador.wGrupoNFSe('/Pagamento');
    end;
  end;

  Gerador.wGrupoNFSe('/Pagamentos');

  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarImpostosEncerramento;
begin
  with CIOT.EncerrarOperacao.Impostos do
  begin
    Gerador.wGrupoNFSe('Impostos', 'AP61');
    Gerador.wCampoNFSe(tcDe2, 'AP62', 'IRRF                   ', 01, 01, 1, IRRF, 'Valor destinado ao IRRF');
    Gerador.wCampoNFSe(tcDe2, 'AP63', 'SestSenat              ', 01, 01, 1, SestSenat, 'Valor destinado ao SEST / SENAT');
    Gerador.wCampoNFSe(tcDe2, 'AP64', 'INSS                   ', 01, 01, 1, INSS, 'Valor destinado ao INSS.');
    Gerador.wCampoNFSe(tcDe2, 'AP65', 'ISSQN                  ', 01, 01, 1, ISSQN, 'Valor destinado ao ISSQN.');
    Gerador.wCampoNFSe(tcDe2, 'AP66', 'OutrosImpostos         ', 01, 01, 1, OutrosImpostos, 'Valor destinado a outros impostos não previstos.');
    Gerador.wCampoNFSe(tcStr, 'AP67', 'DescricaoOutrosImpostos', 01, 01, 0, DescricaoOutrosImpostos);
    Gerador.wGrupoNFSe('/Impostos');
  end;
end;

function TCIOTW_eFrete.GerarXml: Boolean;
var
  i: Integer;
begin
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';

  // Carrega Layout que sera utilizado para gera o txt
  Gerador.LayoutArquivoTXT.Clear;
  Gerador.ArquivoFormatoTXT := '';
  Gerador.Prefixo := 'pef:';

//  VersaoDF := DblToVersaoCIOT(Ok, CIOT.OperacaoTransporte.Versao);
//  versao := VersaoCIOTToInt(VersaoDF);

  case CIOT.Integradora.Operacao of
    opLogin:
      begin
        Gerador.Prefixo := 'log:';
        Gerador.wGrupoNFSe('Login');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('LoginRequest');

        with CIOT.Integradora do
        begin
          Gerador.wCampoNFSe(tcStr, 'AP04', 'Integrador', 01, 01, 0, Integrador);
          Gerador.wCampoNFSe(tcInt, 'AP05', 'Versao    ', 01, 01, 1, 1);
          Gerador.wCampoNFSe(tcStr, 'AP04', 'Usuario   ', 01, 01, 0, Usuario);
          Gerador.wCampoNFSe(tcStr, 'AP04', 'Senha     ', 01, 01, 0, Senha);
        end;

        Gerador.wGrupoNFSe('/LoginRequest');

        Gerador.Prefixo := 'log:';
        Gerador.wGrupoNFSe('/Login');
      end;

    opLogout:
      begin
        Gerador.Prefixo := 'log:';
        Gerador.wGrupoNFSe('Logout');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('LogoutRequest');

        with CIOT.Integradora do
        begin
          Gerador.wCampoNFSe(tcStr, 'AP04', 'Token     ', 01, 01, 0, Token);
          Gerador.wCampoNFSe(tcStr, 'AP04', 'Integrador', 01, 01, 0, Integrador);
          Gerador.wCampoNFSe(tcInt, 'AP05', 'Versao    ', 01, 01, 1, 1);
        end;

        Gerador.wGrupoNFSe('/LogoutRequest');

        Gerador.Prefixo := 'log:';
        Gerador.wGrupoNFSe('/Logout');
      end;

    opGravarProprietario:
      begin
        Gerador.Prefixo := 'prop:';
        Gerador.wGrupoNFSe('Gravar');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('GravarRequest');

        GerarIdentificacao(1);
        GerarGravarProprietario;

        Gerador.wGrupoNFSe('/GravarRequest');

        Gerador.Prefixo := 'prop:';
        Gerador.wGrupoNFSe('/Gravar');
      end;

    opGravarVeiculo:
      begin
        Gerador.Prefixo := 'veic:';
        Gerador.wGrupoNFSe('Gravar');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('GravarRequest');

        GerarIdentificacao(1);

        GerarGravarVeiculo;

        Gerador.wGrupoNFSe('/GravarRequest');

        Gerador.Prefixo := 'veic:';
        Gerador.wGrupoNFSe('/Gravar');
      end;

    opGravarMotorista:
      begin
        Gerador.Prefixo := 'mot:';
        Gerador.wGrupoNFSe('Gravar');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('GravarRequest');

        GerarIdentificacao(1);
        GerarGravarMotorista;

        Gerador.wGrupoNFSe('/GravarRequest');

        Gerador.Prefixo := 'mot:';
        Gerador.wGrupoNFSe('/Gravar');
      end;

    opObterCodigoIOT:
      begin
        Gerador.wGrupoNFSe('ObterCodigoIdentificacaoOperacaoTransportePorIdOperacaoCliente');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('ObterCodigoIdentificacaoOperacaoTransportePorIdOperacaoClienteRequest');

        GerarIdentificacao(1);

        with CIOT.ObterCodigoOperacaoTransporte do
        begin
          Gerador.wCampoNFSe(tcStr, '', 'MatrizCNPJ       ', 14, 14, 0, MatrizCNPJ, '');
          Gerador.wCampoNFSe(tcStr, '', 'IdOperacaoCliente', 01, 30, 0, IdOperacaoCliente, '');
        end;

        Gerador.wGrupoNFSe('/ObterCodigoIdentificacaoOperacaoTransportePorIdOperacaoClienteRequest');

        Gerador.Prefixo := 'pef:';
        Gerador.wGrupoNFSe('/ObterCodigoIdentificacaoOperacaoTransportePorIdOperacaoCliente');
      end;

    opObterPdf:
      begin
        Gerador.wGrupoNFSe('ObterOperacaoTransportePdf');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('ObterOperacaoTransportePdfRequest');

        GerarIdentificacao(1);

        with CIOT.ObterOperacaoTransportePDF do
        begin
          Gerador.wCampoNFSe(tcStr, '', 'CodigoIdentificacaoOperacao', 01, 30, 0, CodigoIdentificacaoOperacao, '');
          Gerador.wCampoNFSe(tcStr, '', 'DocumentoViagem            ', 01, 30, 0, DocumentoViagem, '');
        end;
        Gerador.wGrupoNFSe('/ObterOperacaoTransportePdfRequest');

        Gerador.Prefixo := 'pef:';
        Gerador.wGrupoNFSe('/ObterOperacaoTransportePdf');
      end;

    opAdicionar:
      begin
        Gerador.wGrupoNFSe('AdicionarOperacaoTransporte', '');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('AdicionarOperacaoTransporteRequest', 'AP01');

        GerarIdentificacao(7);

        with CIOT.AdicionarOperacao do
        begin
          Gerador.wCampoNFSe(tcStr, 'AP01', 'TipoViagem            ', 01, 01, 1, TipoViagemCIOTToStr(TipoViagem));
          Gerador.wCampoNFSe(tcStr, 'AP02', 'TipoPagamento         ', 01, 20, 1, TpPagamentoToStr(TipoPagamento));
          Gerador.wCampoNFSe(tcStr, 'AP03', 'BloquearNaoEquiparado ', 01, 01, 1, LowerCase(BoolToStr(BloquearNaoEquiparado, True)));
          Gerador.wCampoNFSe(tcStr, 'AP04', 'MatrizCNPJ            ', 14, 14, 1, MatrizCNPJ);
          Gerador.wCampoNFSe(tcStr, 'AP05', 'FilialCNPJ            ', 14, 14, 1, FilialCNPJ);
          Gerador.wCampoNFSe(tcStr, 'AP06', 'IdOperacaoCliente     ', 01, 01, 0, IdOperacaoCliente, 'Id / Chave primária da operação de transporte no sistema do Cliente.');
          Gerador.wCampoNFSe(tcDat, 'AP07', 'DataInicioViagem      ', 10, 10, 1, DataInicioViagem);
          Gerador.wCampoNFSe(tcDat, 'AP08', 'DataFimViagem         ', 10, 10, 1, DataFimViagem, 'Data prevista para o fim de viagem.');
          Gerador.wCampoNFSe(tcInt, 'AP09', 'CodigoNCMNaturezaCarga', 01, 04, 1, CodigoNCMNaturezaCarga);
          Gerador.wCampoNFSe(tcDe4, 'AP10', 'PesoCarga             ', 01, 01, 1, PesoCarga);
          Gerador.wCampoNFSe(tcStr, 'AP11', 'TipoEmbalagem         ', 01, 01, 1, TipoEmbalagemToStr(TipoEmbalagem));

          if TipoViagem = Padrao then
            GerarViagem;

          if TipoViagem <> Frota then
            GerarImpostos;

          GerarPagamentos;
          GerarContratado;
          GerarMotorista;

          if TipoViagem <> Frota then
            GerarDestinatario;

          GerarContratante;

          if TipoViagem <> Frota then
          begin
            GerarSubContratante;
            GerarConsignatario;
            GerarTomadorServico;
          end;

          GerarRemetente;
          GerarProprietarioCarga;
          GerarVeiculos('adic:');

          //Informar um CIOT (se existente) que esteja relacionado à operação de transporte.
          //Por exemplo: No caso da presença de um Subcontratante na operação de transporte informar
          //o CIOT onde o Subcontratante foi o Contratado.
          Gerador.wCampoNFSe(tcStr, 'AP248', 'CodigoIdentificacaoOperacaoPrincipal', 01, 01, 0, CodigoIdentificacaoOperacaoPrincipal);

          if ObservacoesAoTransportador.Count > 0 then
          begin
            Gerador.wGrupoNFSe('ObservacoesAoTransportador', 'AP249');
            for i := 0 to ObservacoesAoTransportador.Count -1 do
            begin
              with ObservacoesAoTransportador.Items[i] do
              begin
                Gerador.wCampoNFSe(tcStr, 'AP250', 'string', 01, 01, 1, Mensagem);
              end;
            end;
            Gerador.wGrupoNFSe('/ObservacoesAoTransportador');
          end;

          if ObservacoesAoCredenciado.Count > 0 then
          begin
            Gerador.wGrupoNFSe('ObservacoesAoCredenciado', 'AP251');
            for i := 0 to ObservacoesAoCredenciado.Count -1 do
            begin
              with ObservacoesAoCredenciado.Items[i] do
              begin
                Gerador.wCampoNFSe(tcStr, 'AP252', 'string', 01, 01, 1, Mensagem);
              end;
            end;
            Gerador.wGrupoNFSe('/ObservacoesAoCredenciado');
          end;

          Gerador.wCampoNFSe(tcStr, 'AP253', 'EntregaDocumentacao     ', 01, 01, 1, EntregaDocumentacaoToStr(EntregaDocumentacao));
          Gerador.wCampoNFSe(tcInt, 'AP254', 'QuantidadeSaques        ', 01, 01, 1, QuantidadeSaques);
          Gerador.wCampoNFSe(tcInt, 'AP255', 'QuantidadeTransferencias', 01, 01, 1, QuantidadeTransferencias);
          Gerador.wCampoNFSe(tcDe2, 'AP256', 'ValorSaques             ', 01, 01, 1, ValorSaques);
          Gerador.wCampoNFSe(tcDe2, 'AP257', 'ValorTransferencias     ', 01, 01, 1, ValorTransferencias);
          Gerador.wCampoNFSe(tcInt, 'AP258', 'CodigoTipoCarga         ', 01, 01, 1, CodigoTipoCarga);

          Gerador.wCampoNFSe(tcBoolStr, 'AP259', 'AltoDesempenho     ', 01, 01, 1, LowerCase(BoolToStr(AltoDesempenho, True)));
          Gerador.wCampoNFSe(tcBoolStr, 'AP260', 'DestinacaoComercial', 01, 01, 1, LowerCase(BoolToStr(DestinacaoComercial, True)));
          Gerador.wCampoNFSe(tcBoolStr, 'AP261', 'FreteRetorno       ', 01, 01, 1, LowerCase(BoolToStr(FreteRetorno, True)));

          Gerador.wCampoNFSe(tcStr, 'AP262', 'CepRetorno      ', 01, 01, 0, CepRetorno);
          Gerador.wCampoNFSe(tcInt, 'AP263', 'DistanciaRetorno', 01, 01, 1, DistanciaRetorno);
        end;

        Gerador.wGrupoNFSe('/AdicionarOperacaoTransporteRequest');

        Gerador.Prefixo := 'pef:';
        Gerador.wGrupoNFSe('/AdicionarOperacaoTransporte');
      end;

    opRetificar:
      begin
        Gerador.wGrupoNFSe('RetificarOperacaoTransporte');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('RetificarOperacaoTransporteRequest', 'WP01');

        GerarIdentificacao(3);

        with CIOT.RetificarOperacao do
        begin
          Gerador.wCampoNFSe(tcStr, 'WP02', 'CodigoIdentificacaoOperacao', 01, 01, 0, CodigoIdentificacaoOperacao, '');
          Gerador.wCampoNFSe(tcDat, 'WP07', 'DataInicioViagem           ', 01, 01, 1, DataInicioViagem);
          Gerador.wCampoNFSe(tcDat, 'WP06', 'DataFimViagem              ', 01, 01, 1, DataFimViagem);
          Gerador.wCampoNFSe(tcInt, 'WP05', 'CodigoNCMNaturezaCarga     ', 01, 04, 1, CodigoNCMNaturezaCarga);
          Gerador.wCampoNFSe(tcDe4, 'WP09', 'PesoCarga                  ', 01, 01, 1, PesoCarga);
          Gerador.wCampoNFSe(tcInt, 'WP04', 'CodigoMunicipioOrigem      ', 01, 07, 1, CodigoMunicipioOrigem);
          Gerador.wCampoNFSe(tcInt, 'WP03', 'CodigoMunicipioDestino     ', 01, 07, 1, CodigoMunicipioDestino);

          //Adiciona Veículos
          GerarVeiculos('ret:');

          Gerador.wCampoNFSe(tcInt, 'AP209', 'QuantidadeSaques        ', 01, 01, 1, QuantidadeSaques);
          Gerador.wCampoNFSe(tcInt, 'AP210', 'QuantidadeTransferencias', 01, 01, 1, QuantidadeTransferencias);
          Gerador.wCampoNFSe(tcDe2, 'AP211', 'ValorSaques             ', 01, 01, 1, ValorSaques);
          Gerador.wCampoNFSe(tcDe2, 'AP212', 'ValorTransferencias     ', 01, 01, 1, ValorTransferencias);
          Gerador.wCampoNFSe(tcInt, 'AP213', 'CodigoTipoCarga         ', 01, 01, 1, CodigoTipoCarga);
          Gerador.wCampoNFSe(tcStr, 'AP209', 'CepOrigem               ', 01, 01, 0, CepOrigem);
          Gerador.wCampoNFSe(tcStr, 'AP209', 'CepDestino              ', 01, 01, 0, CepDestino);
          Gerador.wCampoNFSe(tcInt, 'AP213', 'DistanciaPercorrida     ', 01, 01, 1, DistanciaPercorrida);
        end;

        Gerador.wGrupoNFSe('/RetificarOperacaoTransporteRequest');

        Gerador.Prefixo := 'pef:';
        Gerador.wGrupoNFSe('/RetificarOperacaoTransporte');
      end;

    opCancelar:
      begin
        Gerador.wGrupoNFSe('CancelarOperacaoTransporte');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('CancelarOperacaoTransporteRequest', 'WP01');

        GerarIdentificacao(1);

        with CIOT.CancelarOperacao do
        begin
          Gerador.wCampoNFSe(tcStr, 'KP02', 'CodigoIdentificacaoOperacao', 01, 01, 0, CodigoIdentificacaoOperacao);
          Gerador.wCampoNFSe(tcStr, 'KP02', 'Motivo                     ', 01, 01, 0, Motivo);
        end;

        Gerador.wGrupoNFSe('/CancelarOperacaoTransporteRequest');

        Gerador.Prefixo := 'pef:';
        Gerador.wGrupoNFSe('/CancelarOperacaoTransporte');
      end;

    opAdicionarViagem:
      begin
        Gerador.wGrupoNFSe('AdicionarViagem');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('AdicionarViagemRequest', 'WP01');

        GerarIdentificacao(3);

        with CIOT.AdicionarViagem do
        begin
          Gerador.wCampoNFSe(tcStr, 'KP02', 'CodigoIdentificacaoOperacao', 01, 01, 0, CodigoIdentificacaoOperacao);

          GerarViagemAdicViagem;
          GerarPagamentosAdicViagem;

          Gerador.wCampoNFSe(tcBoolStr, 'AP259', 'NaoAdicionarParcialmente', 01, 01, 1, LowerCase(BoolToStr(NaoAdicionarParcialmente, True)));
        end;

        Gerador.wGrupoNFSe('/AdicionarViagemRequest');

        Gerador.Prefixo := 'pef:';
        Gerador.wGrupoNFSe('/AdicionarViagem');
      end;

    opAdicionarPagamento:
      begin
        Gerador.wGrupoNFSe('AdicionarPagamento');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('AdicionarPagamentoRequest', 'WP01');

        GerarIdentificacao(3);

        with CIOT.AdicionarPagamento do
        begin
          Gerador.wCampoNFSe(tcStr, 'KP02', 'CodigoIdentificacaoOperacao', 01, 01, 0, CodigoIdentificacaoOperacao);

          GerarPagamentosAdicPagamento;
        end;

        Gerador.wGrupoNFSe('/AdicionarPagamentoRequest');

        Gerador.Prefixo := 'pef:';
        Gerador.wGrupoNFSe('/AdicionarPagamento');
      end;

    opCancelarPagamento:
      begin
        Gerador.wGrupoNFSe('CancelarPagamento');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('CancelarPagamentoRequest', 'WP01');

        GerarIdentificacao(2);

        with CIOT.CancelarPagamento do
        begin
          Gerador.wCampoNFSe(tcStr, 'KP02', 'CodigoIdentificacaoOperacao', 01, 01, 0, CodigoIdentificacaoOperacao);
          Gerador.wCampoNFSe(tcStr, 'AP69', 'IdPagamentoCliente         ', 01, 01, 0, IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente.');
          Gerador.wCampoNFSe(tcStr, 'KP02', 'Motivo                     ', 01, 01, 0, Motivo);
        end;

        Gerador.wGrupoNFSe('/CancelarPagamentoRequest');

        Gerador.Prefixo := 'pef:';
        Gerador.wGrupoNFSe('/CancelarPagamento');
      end;

    opEncerrar:
      begin
        Gerador.wGrupoNFSe('EncerrarOperacaoTransporte');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('EncerrarOperacaoTransporteRequest ', 'WP01');

        GerarIdentificacao(2);

        with CIOT.EncerrarOperacao do
        begin
          Gerador.wCampoNFSe(tcStr, 'QP02', 'CodigoIdentificacaoOperacao', 01, 01, 0, CodigoIdentificacaoOperacao);
          Gerador.wCampoNFSe(tcDe6, 'QP03', 'PesoCarga                  ', 01, 01, 1, PesoCarga);

          GerarViagemEncerramento;
          GerarPagamentosEncerramento;
          GerarImpostosEncerramento;

          Gerador.wCampoNFSe(tcInt, 'AP254', 'QuantidadeSaques        ', 01, 01, 1, QuantidadeSaques);
          Gerador.wCampoNFSe(tcInt, 'AP255', 'QuantidadeTransferencias', 01, 01, 1, QuantidadeTransferencias);
          Gerador.wCampoNFSe(tcDe2, 'AP256', 'ValorSaques             ', 01, 01, 1, ValorSaques);
          Gerador.wCampoNFSe(tcDe2, 'AP257', 'ValorTransferencias     ', 01, 01, 1, ValorTransferencias);
        end;

        Gerador.wGrupoNFSe('/EncerrarOperacaoTransporteRequest');

        Gerador.Prefixo := 'pef:';
        Gerador.wGrupoNFSe('/EncerrarOperacaoTransporte');
      end;
  end;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
