
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

  NAME_SPACE_BASE = 'xmlns="http://schemas.ipc.adm.br/efrete/pef"';

  NAME_SPACE_EFRETE_OBJECTS = 'xmlns="http://schemas.ipc.adm.br/efrete/objects"';
  NAME_SPACE_EFRETE_PEFOBTER_OBJECTS = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/ObterOperacaoTransporteObjects"';
  NAME_SPACE_EFRETE_PEFADICIONAR_OBJECTS = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/AdicionarOperacaoTransporte"';
  NAME_SPACE_EFRETE_PEFADICIONAR_VIAGEM = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/AdicionarViagem"';
  NAME_SPACE_EFRETE_PEFADICIONAR_PAGAMENTOS = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/AdicionarPagamento"';
  NAME_SPACE_EFRETE_PEFENCERRAR_OPERACAO = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/EncerrarOperacaoTransporte"';

  NAME_SPACE_EFRETE_PEFRETIFICAR_OBJECTS = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/RetificarOperacaoTransporte"';
  NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/objects"';

  NAME_SPACE_EFRETE_VEICULOS_EFRETE = 'xmlns="http://schemas.ipc.adm.br/efrete/veiculos/objects"';
  NAME_SPACE_EFRETE_MOTORISTAS_EFRETE = 'xmlns="http://schemas.ipc.adm.br/efrete/motoristas/objects"';
  NAME_SPACE_EFRETE_PROPRIETARIOS_EFRETE = 'xmlns="http://schemas.ipc.adm.br/efrete/proprietarios/objects"';

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
  Gerador.wCampoNFSe(tcInt, 'AP05', 'Versao', 01, 01, 1, versao);

  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarViagem;
var
  i, j: Integer;
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := 'adic:';

  Gerador.wGrupoNFSe('Viagens '{+NAME_SPACE_EFRETE_PEFADICIONAR_OBJECTS}, 'AP16');

  for I := 0 to CIOT.OperacaoTransporte.Viagens.Count -1 do
  begin
    with CIOT.OperacaoTransporte.Viagens.Items[I] do
    begin
      Gerador.wCampoNFSe(tcStr, 'AP17', 'DocumentoViagem', 01, 01, 1, DocumentoViagem, 'Exemplo: CT-e / Serie, CTRC / Serie, Ordem de Serviço.');
      Gerador.wCampoNFSe(tcInt, 'AP18', 'CodigoMunicipioOrigem', 01, 07, 1, CodigoMunicipioOrigem);
      Gerador.wCampoNFSe(tcInt, 'AP19', 'CodigoMunicipioDestino', 01, 07, 1, CodigoMunicipioDestino);

      Gerador.wCampoNFSe(tcInt, 'AP19', 'DistanciaPercorrida', 01, 07, 1, 0);

      Gerador.Prefixo := 'obj:';
      Gerador.wGrupoNFSe('Valores '{+NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP20');

      with Valores do
      begin
        Gerador.wCampoNFSe(tcDe2, 'AP21', 'TotalOperacao', 01, 01, 1, TotalOperacao);
        Gerador.wCampoNFSe(tcDe2, 'AP22', 'TotalViagem', 01, 01, 1, TotalViagem);
        Gerador.wCampoNFSe(tcDe2, 'AP23', 'TotalDeAdiantamento', 01, 01, 1, TotalDeAdiantamento);
        Gerador.wCampoNFSe(tcDe2, 'AP24', 'TotalDeQuitacao', 01, 01, 1, TotalDeQuitacao);
        Gerador.wCampoNFSe(tcDe2, 'AP25', 'Combustivel', 01, 01, 1, Combustivel);
        Gerador.wCampoNFSe(tcDe2, 'AP26', 'Pedagio', 01, 01, 1, Pedagio);
        Gerador.wCampoNFSe(tcDe2, 'AP27', 'OutrosCreditos', 01, 01, 1, OutrosCreditos);
        Gerador.wCampoNFSe(tcStr, 'AP28', 'JustificativaOutrosCreditos', 01, 01, 0, JustificativaOutrosCreditos);
        Gerador.wCampoNFSe(tcDe2, 'AP29', 'Seguro', 01, 01, 1, Seguro);
        Gerador.wCampoNFSe(tcDe2, 'AP30', 'OutrosDebitos', 01, 01, 1, OutrosDebitos);
        Gerador.wCampoNFSe(tcStr, 'AP31', 'JustificativaOutrosDebitos', 01, 01, 0, JustificativaOutrosDebitos);
      end;
      Gerador.wGrupoNFSe('/Valores');

      Gerador.wCampoNFSe(tcStr, 'AP32', 'TipoPagamento' {+NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 001, 020, 1, TpPagamentoToStr(TipoPagamento), 'Tipo de pagamento que será usado pelo contratante. Restrito aos itens da enum: -TransferenciaBancaria -eFRETE');

      // Falta informações Bancarias

      Gerador.Prefixo := 'adic:';
      Gerador.wGrupoNFSe('NotasFiscais', 'AP33');

      for J := 0 to NotasFiscais.Count -1 do
      begin
        with NotasFiscais.Items[J] do
        begin
          Gerador.wGrupoNFSe('NotaFiscal', 'AP34');
          Gerador.wCampoNFSe(tcStr, 'AP35', 'Numero', 01, 01, 0, Numero);
          Gerador.wCampoNFSe(tcStr, 'AP36', 'Serie', 01, 01, 0, Serie);
          Gerador.wCampoNFSe(tcDat, 'AP37', 'Data', 01, 01, 1, Data);
          Gerador.wCampoNFSe(tcDe2, 'AP38', 'ValorTotal', 01, 01, 1, ValorTotal);
          Gerador.wCampoNFSe(tcDe4, 'AP39', 'ValorDaMercadoriaPorUnidade', 01, 01, 1, ValorDaMercadoriaPorUnidade);
          Gerador.wCampoNFSe(tcInt, 'AP40', 'CodigoNCMNaturezaCarga', 01, 04, 1, CodigoNCMNaturezaCarga);
          Gerador.wCampoNFSe(tcStr, 'AP41', 'DescricaoDaMercadoria', 01, 01, 0, DescricaoDaMercadoria, 'Descrição adicional ao código NCM.');
          Gerador.wCampoNFSe(tcStr, 'AP42', 'UnidadeDeMedidaDaMercadoria', 01, 01, 1, TpUnMedMercToStr(UnidadeDeMedidaDaMercadoria));
          Gerador.wCampoNFSe(tcStr, 'AP43', 'TipoDeCalculo', 01, 01, 1, TpVgTipoCalculoToStr(TipoDeCalculo));
          Gerador.wCampoNFSe(tcDe4, 'AP44', 'ValorDoFretePorUnidadeDeMercadoria', 01, 01, 1, ValorDoFretePorUnidadeDeMercadoria);
          Gerador.wCampoNFSe(tcDe4, 'AP45', 'QuantidadeDaMercadoriaNoEmbarque', 01, 01, 1, QuantidadeDaMercadoriaNoEmbarque);

          if ToleranciaDePerdaDeMercadoria.Valor > 0 then
          begin
            Gerador.wGrupoNFSe('ToleranciaDePerdaDeMercadoria', 'AP46');
            Gerador.wCampoNFSe(tcStr, 'AP47', 'Tipo', 01, 01, 1, TpProporcaoToStr(ToleranciaDePerdaDeMercadoria.Tipo));
            Gerador.wCampoNFSe(tcDe2, 'AP48', 'Valor', 01, 01, 1, ToleranciaDePerdaDeMercadoria.Valor);
            Gerador.wGrupoNFSe('/ToleranciaDePerdaDeMercadoria');
          end;

          if DiferencaDeFrete.Tipo <> SemDiferenca then
          begin
            Gerador.wGrupoNFSe('DiferencaDeFrete', 'AP49');
            Gerador.Prefixo := 'obj:';

            Gerador.wCampoNFSe(tcStr, 'AP50', 'Tipo', 01, 01, 1, TpDifFreteToStr(DiferencaDeFrete.Tipo));
            Gerador.wCampoNFSe(tcStr, 'AP51', 'Base', 01, 01, 1, TpDiferencaFreteBCToStr(DiferencaDeFrete.Base));

            if DiferencaDeFrete.Tolerancia.Valor > 0 then
            begin
              Gerador.wGrupoNFSe('Tolerancia', 'AP52');
              Gerador.wCampoNFSe(tcStr, 'AP53', 'Tipo', 01, 01, 1, TpProporcaoToStr(DiferencaDeFrete.Tolerancia.Tipo));
              Gerador.wCampoNFSe(tcDe2, 'AP54', 'Valor', 01, 01, 1, DiferencaDeFrete.Tolerancia.Valor);
              Gerador.wGrupoNFSe('/Tolerancia');
            end;

            if DiferencaDeFrete.MargemGanho.Valor > 0 then
            begin
              Gerador.wGrupoNFSe('MargemGanho', 'AP55');
              Gerador.wCampoNFSe(tcStr, 'AP56', 'Tipo', 01, 01, 1, TpProporcaoToStr(DiferencaDeFrete.MargemGanho.Tipo));
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

  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarImpostos;
begin
  Gerador.wGrupoNFSe('Impostos', 'AP61');
  Gerador.wCampoNFSe(tcDe2, 'AP62', 'IRRF', 01, 01, 1, CIOT.OperacaoTransporte.Impostos.IRRF, 'Valor destinado ao IRRF');
  Gerador.wCampoNFSe(tcDe2, 'AP63', 'SestSenat', 01, 01, 1, CIOT.OperacaoTransporte.Impostos.SestSenat, 'Valor destinado ao SEST / SENAT');
  Gerador.wCampoNFSe(tcDe2, 'AP64', 'INSS', 01, 01, 1, CIOT.OperacaoTransporte.Impostos.INSS, 'Valor destinado ao INSS.');
  Gerador.wCampoNFSe(tcDe2, 'AP65', 'ISSQN', 01, 01, 1, CIOT.OperacaoTransporte.Impostos.ISSQN, 'Valor destinado ao ISSQN.');
  Gerador.wCampoNFSe(tcDe2, 'AP66', 'OutrosImpostos', 01, 01, 1, CIOT.OperacaoTransporte.Impostos.OutrosImpostos, 'Valor destinado a outros impostos não previstos.');
  Gerador.wCampoNFSe(tcStr, 'AP67', 'DescricaoOutrosImpostos', 01, 01, 0, CIOT.OperacaoTransporte.Impostos.DescricaoOutrosImpostos);
  Gerador.wGrupoNFSe('/Impostos');
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
  for i := 0 to CIOT.OperacaoTransporte.Pagamentos.Count -1 do
  begin
    with CIOT.OperacaoTransporte.Pagamentos.Items[i] do
    begin
      Gerador.wGrupoNFSe('Pagamentos'{+NAME_SPACE_EFRETE_PEFADICIONAR_OBJECTS}, 'AP68');
      Gerador.wCampoNFSe(tcStr, 'AP69', 'IdPagamentoCliente', 01, 01, 0, IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente.');
      Gerador.wCampoNFSe(tcDat, 'AP70', 'DataDeLiberacao', 01, 01, 1, DataDeLiberacao, 'Data em que o pagamento será liberado para saque.');
      Gerador.wCampoNFSe(tcDe2, 'AP71', 'Valor', 01, 01, 1, Valor, 'Valor do pagamento.');

      Gerador.Prefixo := 'obj:';
      Gerador.wCampoNFSe(tcStr, 'AP72', 'TipoPagamento', 01, 01, 1, TpPagamentoToStr(TipoPagamento), 'Tipo de pagamento que será usado pelo contratante. Restrito aos itens da enum: -TransferenciaBancaria -eFRETE', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
      Gerador.wCampoNFSe(tcStr, 'AP73', 'Categoria', 01, 01, 1, TpCatPagToStr(Categoria), 'Categoria relacionada ao pagamento realizado. Restrita aos membros da ENUM: -Adiantamento, -Estadia, -Quitacao, -SemCategoria, -Frota ', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});

      Gerador.Prefixo := 'adic:';
      Gerador.wCampoNFSe(tcStr, 'AP74', 'Documento', 01, 01, 0, Documento, 'Documento relacionado a viagem.');

      //Preenchimento obrigatório para o TipoPagamento TransferenciaBancaria. Não deve ser preenchido para TipoPagamento eFRETE.
      with InformacoesBancarias do
      begin
        if (InstituicaoBancaria <> '') or (Agencia <> '') or (Conta <> '') then
        begin
          Gerador.Prefixo := 'obj:';
          Gerador.wGrupoNFSe('InformacoesBancarias'{+NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP75');

          Gerador.wCampoNFSe(tcStr, 'AP76', 'InstituicaoBancaria', 01, 01, 0, InstituicaoBancaria, 'Código de compensação da instituição bancária que será realizado o pagamento. ');
          Gerador.wCampoNFSe(tcStr, 'AP77', 'Agencia', 01, 01, 0, Agencia, 'Agência na qual o contratado possui conta com dígito (se houver).');
          Gerador.wCampoNFSe(tcStr, 'AP78', 'Conta', 01, 01, 0, Conta, 'Conta do contratado com dígito. ');

          Gerador.wGrupoNFSe('/InformacoesBancarias');
        end;
      end;

      Gerador.wCampoNFSe(tcStr, 'AP79', 'InformacaoAdicional', 01, 01, 0, InformacaoAdicional);

      if Categoria = tcpFrota then
        Gerador.wCampoNFSe(tcStr, 'AP80', 'CnpjFilialAbastecimento', 01, 01, 1, CnpjFilialAbastecimento);

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
  Gerador.wGrupoNFSe('Contratado'{+NAME_SPACE_EFRETE_PEFADICIONAR_OBJECTS}, 'AP81');
  Gerador.wCampoNFSe(tcStr, 'AP82', 'CpfOuCnpj', 01, 01, 1, CIOT.OperacaoTransporte.Contratado.CpfOuCnpj);
  Gerador.wCampoNFSe(tcStr, 'AP83', 'RNTRC', 01, 01, 1, CIOT.OperacaoTransporte.Contratado.RNTRC);
  Gerador.wGrupoNFSe('/Contratado');

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
  Gerador.wGrupoNFSe('Motorista'{+NAME_SPACE_EFRETE_PEFADICIONAR_OBJECTS}, 'AP84');
  Gerador.wCampoNFSe(tcStr, 'AP85', 'CpfOuCnpj', 01, 11, 1, CIOT.OperacaoTransporte.Motorista.CpfOuCnpj, 'CPF ou CNPJ do Motorista.');
  Gerador.wCampoNFSe(tcStr, 'AP86', 'CNH', 01, 11, 1, CIOT.OperacaoTransporte.Motorista.CNH);

  Gerador.Prefixo := 'obj:';

  Gerador.wGrupoNFSe('Celular'{+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP87');
  with CIOT.OperacaoTransporte.Motorista do
  begin
    Gerador.Prefixo := 'obj1:';
    Gerador.wCampoNFSe(tcInt, 'AP88', 'DDD', 01, 02, 1, Celular.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
    Gerador.wCampoNFSe(tcInt, 'AP89', 'Numero', 08, 09, 1, Celular.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
  end;

  Gerador.Prefixo := 'obj:';
  Gerador.wGrupoNFSe('/Celular');
  Gerador.Prefixo := 'adic:';
  Gerador.wGrupoNFSe('/Motorista');

  Gerador.Prefixo := aPrefixo;
end;

procedure TCIOTW_eFrete.GerarDestinatario;
begin
  //Destinatário da carga.
  //Na emissão com TipoViagem Padrão seu preenchimento é obrigatório.
  //Na emissão com TipoViagem TAC_Agregado o campo não deve ser preenchido.
  //Não esperado para TipoViagem Frota.
  if Length(Trim(CIOT.OperacaoTransporte.Destinatario.CpfOuCnpj)) > 0 then
  begin
    Gerador.wGrupoNFSe('Destinatario', 'AP90');
    Gerador.wCampoNFSe(tcStr, 'AP91', 'NomeOuRazaoSocial', 01, 01, 1, CIOT.OperacaoTransporte.Destinatario.NomeOuRazaoSocial);
    Gerador.wCampoNFSe(tcStr, 'AP92', 'CpfOuCnpj', 11, 14, 1, CIOT.OperacaoTransporte.Destinatario.CpfOuCnpj);

    with CIOT.OperacaoTransporte.Destinatario do
    begin
      Gerador.wGrupoNFSe('Endereco', 'AP93');
      Gerador.Prefixo := 'obj1:';
      Gerador.wCampoNFSe(tcStr, 'AP94', 'Bairro', 01, 01, 1, Endereco.Bairro);
      Gerador.wCampoNFSe(tcStr, 'AP95', 'Rua', 01, 01, 1, Endereco.Rua);
      Gerador.wCampoNFSe(tcStr, 'AP96', 'Numero', 01, 01, 1, Endereco.Numero);
      Gerador.wCampoNFSe(tcStr, 'AP97', 'Complemento', 01, 01, 1, Endereco.Complemento);
      Gerador.wCampoNFSe(tcStr, 'AP98', 'CEP', 08, 08, 1, Endereco.CEP);
      Gerador.wCampoNFSe(tcInt, 'AP99', 'CodigoMunicipio', 07, 07, 1, Endereco.CodigoMunicipio);
      Gerador.Prefixo := 'obj:';
      Gerador.wGrupoNFSe('/Endereco');
    end;

    Gerador.wCampoNFSe(tcStr, 'AP100', 'EMail', 01, 01, 0, CIOT.OperacaoTransporte.Destinatario.EMail);

    with CIOT.OperacaoTransporte.Destinatario.Telefones do
    begin
      Gerador.wGrupoNFSe('Telefones', 'AP101');

      Gerador.Prefixo := 'obj1:';
      Gerador.wGrupoNFSe('Celular'{+NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP102');
      Gerador.wCampoNFSe(tcInt, 'AP103', 'DDD', 01, 02, 1, Celular.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
      Gerador.wCampoNFSe(tcInt, 'AP104', 'Numero', 08, 09, 1, Celular.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
      Gerador.wGrupoNFSe('/Celular');

      Gerador.wGrupoNFSe('Fixo'{+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP105');
      Gerador.wCampoNFSe(tcInt, 'AP106', 'DDD', 01, 02, 1, Fixo.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
      Gerador.wCampoNFSe(tcInt, 'AP107', 'Numero', 08, 09, 1, Fixo.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
      Gerador.wGrupoNFSe('/Fixo');

      Gerador.wGrupoNFSe('Fax' {+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP108');
      Gerador.wCampoNFSe(tcInt, 'AP109', 'DDD', 01, 02, 1, Fax.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
      Gerador.wCampoNFSe(tcInt, 'AP110', 'Numero', 08, 09, 1, Fax.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
      Gerador.wGrupoNFSe('/Fax');

      Gerador.Prefixo := 'obj:';
      Gerador.wGrupoNFSe('/Telefones');
    end;

    Gerador.wCampoNFSe(tcStr, 'AP111', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(CIOT.OperacaoTransporte.Destinatario.ResponsavelPeloPagamento, true)), 'Informar se é o responsável pelo pagamento da Operação de Transporte. True = Sim. False = Não');

    Gerador.wGrupoNFSe('/Destinatario');
  end;
end;

procedure TCIOTW_eFrete.GerarContratante;
begin
  Gerador.wGrupoNFSe('Contratante', 'AP112');
  Gerador.wCampoNFSe(tcStr, 'AP113', 'RNTRC', 01, 01, 1, CIOT.OperacaoTransporte.Contratante.RNTRC);
  Gerador.wCampoNFSe(tcStr, 'AP114', 'NomeOuRazaoSocial', 01, 01, 1, CIOT.OperacaoTransporte.Contratante.NomeOuRazaoSocial);
  Gerador.wCampoNFSe(tcStr, 'AP115', 'CpfOuCnpj', 11, 14, 1, CIOT.OperacaoTransporte.Contratante.CpfOuCnpj);

  with CIOT.OperacaoTransporte.Contratante do
  begin
    Gerador.wGrupoNFSe('Endereco', 'AP116');
    Gerador.Prefixo := 'obj1:';
    Gerador.wCampoNFSe(tcStr, 'AP117', 'Bairro', 01, 01, 0, Endereco.Bairro);
    Gerador.wCampoNFSe(tcStr, 'AP118', 'Rua', 01, 01, 0, Endereco.Rua);
    Gerador.wCampoNFSe(tcStr, 'AP119', 'Numero', 01, 01, 0, Endereco.Numero);
    Gerador.wCampoNFSe(tcStr, 'AP120', 'Complemento', 01, 01, 0, Endereco.Complemento);
    Gerador.wCampoNFSe(tcStr, 'AP121', 'CEP', 01, 09, 0, Endereco.CEP);
    Gerador.wCampoNFSe(tcInt, 'AP122', 'CodigoMunicipio', 07, 07, 1, Endereco.CodigoMunicipio);
    Gerador.Prefixo := 'obj:';
    Gerador.wGrupoNFSe('/Endereco');
  end;

  Gerador.wCampoNFSe(tcStr, 'AP123', 'EMail', 01, 01, 0, CIOT.OperacaoTransporte.Contratante.EMail);

  with CIOT.OperacaoTransporte.Contratante.Telefones do
  begin
    if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
    begin
      Gerador.wGrupoNFSe('Telefones', 'AP124');

      Gerador.Prefixo := 'obj1:';

      if Celular.Numero > 0 then
      begin
        Gerador.wGrupoNFSe('Celular'{+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP125');
        Gerador.wCampoNFSe(tcInt, 'AP126', 'DDD', 01, 02, 0, Celular.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
        Gerador.wCampoNFSe(tcInt, 'AP127', 'Numero', 08, 09, 0, Celular.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
        Gerador.wGrupoNFSe('/Celular');
      end;

      if Fixo.Numero > 0 then
      begin
        Gerador.wGrupoNFSe('Fixo' {+NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP128');
        Gerador.wCampoNFSe(tcInt, 'AP129', 'DDD', 01, 02, 0, Fixo.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
        Gerador.wCampoNFSe(tcInt, 'AP130', 'Numero', 08, 09, 0, Fixo.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
        Gerador.wGrupoNFSe('/Fixo');
      end;

      if Fax.Numero > 0 then
      begin
        Gerador.wGrupoNFSe('Fax'{+NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP131');
        Gerador.wCampoNFSe(tcInt, 'AP132', 'DDD', 01, 02, 0, Fax.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
        Gerador.wCampoNFSe(tcInt, 'AP133', 'Numero', 08, 09, 0, Fax.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
        Gerador.wGrupoNFSe('/Fax');
      end;

      Gerador.Prefixo := 'obj:';
      Gerador.wGrupoNFSe('/Telefones');
    end;
  end;

  Gerador.wCampoNFSe(tcStr, 'AP134', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(CIOT.OperacaoTransporte.Contratante.ResponsavelPeloPagamento, true)));

  Gerador.wGrupoNFSe('/Contratante');
end;

procedure TCIOTW_eFrete.GerarSubContratante;
begin
  //É o transportador que contratar outro transportador para realização do transporte de
  //cargas para o qual fora anteriormente contratado, indicado no cadastramento da Operação de Transporte.
  //Não esperado para TipoViagem Frota.
  if Length(Trim(CIOT.OperacaoTransporte.Subcontratante.CpfOuCnpj)) > 0 then
  begin
    Gerador.wGrupoNFSe('Subcontratante', 'AP135');
    Gerador.wCampoNFSe(tcStr, 'AP136', 'NomeOuRazaoSocial', 01, 01, 1, CIOT.OperacaoTransporte.Subcontratante.NomeOuRazaoSocial);
    Gerador.wCampoNFSe(tcStr, 'AP137', 'CpfOuCnpj', 01, 01, 1, CIOT.OperacaoTransporte.Subcontratante.CpfOuCnpj);

    with CIOT.OperacaoTransporte.Subcontratante do
    begin
      Gerador.wGrupoNFSe('Endereco', 'AP138');
      Gerador.Prefixo := 'obj1:';
      Gerador.wCampoNFSe(tcStr, 'AP139', 'Bairro', 01, 01, 0, Endereco.Bairro);
      Gerador.wCampoNFSe(tcStr, 'AP140', 'Rua', 01, 01, 0, Endereco.Rua);
      Gerador.wCampoNFSe(tcStr, 'AP141', 'Numero', 01, 01, 0, Endereco.Numero);
      Gerador.wCampoNFSe(tcStr, 'AP142', 'Complemento', 01, 01, 0, Endereco.Complemento);
      Gerador.wCampoNFSe(tcStr, 'AP143', 'CEP', 01, 09, 0, Endereco.CEP);
      Gerador.wCampoNFSe(tcInt, 'AP144', 'CodigoMunicipio', 01, 07, 1, Endereco.CodigoMunicipio);
      Gerador.Prefixo := 'obj:';
      Gerador.wGrupoNFSe('/Endereco');
    end;

    Gerador.wCampoNFSe(tcStr, 'AP145', 'EMail', 01, 01, 0, CIOT.OperacaoTransporte.Subcontratante.EMail);

    with CIOT.OperacaoTransporte.Subcontratante.Telefones do
    begin
      if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
      begin
        Gerador.wGrupoNFSe('Telefones', 'AP146');

        Gerador.Prefixo := 'obj1:';

        if Celular.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Celular'{+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP147');
          Gerador.wCampoNFSe(tcInt, 'AP148', 'DDD', 01, 02, 1, Celular.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wCampoNFSe(tcInt, 'AP149', 'Numero', 08, 09, 1, Celular.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wGrupoNFSe('/Celular');
        end;

        if Fixo.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Fixo'{+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP150');
          Gerador.wCampoNFSe(tcInt, 'AP151', 'DDD', 01, 02, 1, Fixo.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wCampoNFSe(tcInt, 'AP152', 'Numero', 08, 09, 1, Fixo.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wGrupoNFSe('/Fixo');
        end;

        if Fax.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Fax'{+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP153');
          Gerador.wCampoNFSe(tcInt, 'AP154', 'DDD', 01, 02, 1, Fax.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wCampoNFSe(tcInt, 'AP155', 'Numero', 08, 09, 1, Fax.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wGrupoNFSe('/Fax');
        end;

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('/Telefones');
      end;
    end;

    Gerador.wCampoNFSe(tcStr, 'AP156', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(CIOT.OperacaoTransporte.Subcontratante.ResponsavelPeloPagamento, true)));

    Gerador.wGrupoNFSe('/Subcontratante');
  end;
end;

procedure TCIOTW_eFrete.GerarConsignatario;
begin
  //Aquele que receberá as mercadorias transportadas em consignação,
  //indicado no cadastramento da Operação de Transporte ou nos respectivos documentos fiscais.
  //Não esperado para TipoViagem Frota.
  if Length(Trim(CIOT.OperacaoTransporte.Consignatario.CpfOuCnpj)) > 0 then
  begin
    Gerador.wGrupoNFSe('Consignatario', 'AP157');
    Gerador.wCampoNFSe(tcStr, 'AP158', 'NomeOuRazaoSocial', 01, 01, 1, CIOT.OperacaoTransporte.Consignatario.NomeOuRazaoSocial);
    Gerador.wCampoNFSe(tcStr, 'AP159', 'CpfOuCnpj', 01, 01, 1, CIOT.OperacaoTransporte.Consignatario.CpfOuCnpj);

    with CIOT.OperacaoTransporte.Consignatario do
    begin
      Gerador.wGrupoNFSe('Endereco', 'AP160');
      Gerador.Prefixo := 'obj1:';
      Gerador.wCampoNFSe(tcStr, 'AP161', 'Bairro', 01, 01, 0, Endereco.Bairro);
      Gerador.wCampoNFSe(tcStr, 'AP162', 'Rua', 01, 01, 0, Endereco.Rua);
      Gerador.wCampoNFSe(tcStr, 'AP163', 'Numero', 01, 01, 0, Endereco.Numero);
      Gerador.wCampoNFSe(tcStr, 'AP164', 'Complemento', 01, 01, 0, Endereco.Complemento);
      Gerador.wCampoNFSe(tcStr, 'AP165', 'CEP', 01, 09, 0, Endereco.CEP);
      Gerador.wCampoNFSe(tcInt, 'AP166', 'CodigoMunicipio', 01, 07, 1, Endereco.CodigoMunicipio);
      Gerador.Prefixo := 'obj:';
      Gerador.wGrupoNFSe('/Endereco');
    end;

    Gerador.wCampoNFSe(tcStr, 'AP167', 'EMail', 01, 01, 0, CIOT.OperacaoTransporte.Consignatario.EMail);

    with CIOT.OperacaoTransporte.Consignatario.Telefones do
    begin
      if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
      begin
        Gerador.wGrupoNFSe('Telefones', 'AP168');

        Gerador.Prefixo := 'obj1:';

        if Celular.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Celular'{+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP169');
          Gerador.wCampoNFSe(tcInt, 'AP170', 'DDD', 01, 02, 1, Celular.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wCampoNFSe(tcInt, 'AP171', 'Numero', 08, 09, 1, Celular.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wGrupoNFSe('/Celular');
        end;

        if Fixo.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Fixo'{+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP172');
          Gerador.wCampoNFSe(tcInt, 'AP173', 'DDD', 01, 02, 1, Fixo.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wCampoNFSe(tcInt, 'AP174', 'Numero', 08, 09, 1, Fixo.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wGrupoNFSe('/Fixo');
        end;

        if Fax.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Fax'{+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP175');
          Gerador.wCampoNFSe(tcInt, 'AP176', 'DDD', 01, 02, 1, Fax.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wCampoNFSe(tcInt, 'AP177', 'Numero', 08, 09, 1, Fax.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wGrupoNFSe('/Fax');
        end;

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('/Telefones');
      end;
    end;

    Gerador.wCampoNFSe(tcStr, 'AP178', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(CIOT.OperacaoTransporte.Consignatario.ResponsavelPeloPagamento, true)));

    Gerador.wGrupoNFSe('/Consignatario');
  end;
end;

procedure TCIOTW_eFrete.GerarTomadorServico;
begin
  //Pessoa (física ou jurídica) que contratou o frete pela transportadora.
  //Na emissão com TipoViagem Padrão seu preenchimento é obrigatório.
  //Na emissão com TipoViagem TAC_Agregado o campo não deve ser preenchido.
  if Length(Trim(CIOT.OperacaoTransporte.TomadorServico.CpfOuCnpj)) > 0 then
  begin
    Gerador.wGrupoNFSe('TomadorServico', 'AP179');
    Gerador.wCampoNFSe(tcStr, 'AP180', 'NomeOuRazaoSocial', 01, 01, 1, CIOT.OperacaoTransporte.TomadorServico.NomeOuRazaoSocial);
    Gerador.wCampoNFSe(tcStr, 'AP181', 'CpfOuCnpj', 01, 01, 1, CIOT.OperacaoTransporte.TomadorServico.CpfOuCnpj);

    with CIOT.OperacaoTransporte.TomadorServico do
    begin
      Gerador.wGrupoNFSe('Endereco', 'AP182');
      Gerador.Prefixo := 'obj1:';
      Gerador.wCampoNFSe(tcStr, 'AP183', 'Bairro', 01, 01, 0, Endereco.Bairro);
      Gerador.wCampoNFSe(tcStr, 'AP184', 'Rua', 01, 01, 0, Endereco.Rua);
      Gerador.wCampoNFSe(tcStr, 'AP185', 'Numero', 01, 01, 0, Endereco.Numero);
      Gerador.wCampoNFSe(tcStr, 'AP186', 'Complemento', 01, 01, 0, Endereco.Complemento);
      Gerador.wCampoNFSe(tcStr, 'AP187', 'CEP', 01, 09, 0, Endereco.CEP);
      Gerador.wCampoNFSe(tcInt, 'AP188', 'CodigoMunicipio', 01, 07, 1, Endereco.CodigoMunicipio);
      Gerador.Prefixo := 'obj:';
      Gerador.wGrupoNFSe('/Endereco');
    end;

    Gerador.wCampoNFSe(tcStr, 'AP189', 'EMail', 01, 01, 0, CIOT.OperacaoTransporte.TomadorServico.EMail);

    with CIOT.OperacaoTransporte.TomadorServico.Telefones do
    begin
      if (Celular.Numero > 0) or (Fixo.Numero > 0) or (Fax.Numero > 0) then
      begin
        Gerador.wGrupoNFSe('Telefones', 'AP190');

        Gerador.Prefixo := 'obj1:';

        if Celular.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Celular'{+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP191');
          Gerador.wCampoNFSe(tcInt, 'AP192', 'DDD', 01, 02, 1, Celular.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wCampoNFSe(tcInt, 'AP193', 'Numero', 08, 09, 1, Celular.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wGrupoNFSe('/Celular');
        end;

        if Fixo.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Fixo'{+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP194');
          Gerador.wCampoNFSe(tcInt, 'AP195', 'DDD', 01, 02, 1, Fixo.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wCampoNFSe(tcInt, 'AP196', 'Numero', 08, 09, 1, Fixo.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wGrupoNFSe('/Fixo');
        end;

        if Fax.Numero > 0 then
        begin
          Gerador.wGrupoNFSe('Fax'{+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP197');
          Gerador.wCampoNFSe(tcInt, 'AP198', 'DDD', 01, 02, 1, Fax.DDD, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wCampoNFSe(tcInt, 'AP199', 'Numero', 08, 09, 1, Fax.Numero, '', True{, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE});
          Gerador.wGrupoNFSe('/Fax');
        end;

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('/Telefones');
      end;
    end;

    Gerador.wCampoNFSe(tcStr, 'AP200', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(CIOT.OperacaoTransporte.TomadorServico.ResponsavelPeloPagamento, true)));

    Gerador.wGrupoNFSe('/TomadorServico');
  end;
end;

procedure TCIOTW_eFrete.GerarVeiculos;
var
  i: Integer;
  aPrefixo: string;
begin
  aPrefixo := Gerador.Prefixo;
  Gerador.Prefixo := 'adic:';

  //Registro dos veículos participantes da operação de transporte.
  for i := 0 to CIOT.OperacaoTransporte.Veiculos.Count -1 do
  begin
    Gerador.wGrupoNFSe('Veiculos'{+ NAME_SPACE_EFRETE_PEFADICIONAR_OBJECTS}, 'AP201');
    Gerador.wCampoNFSe(tcStr, 'AP202', 'Placa', 01, 07, 1, CIOT.OperacaoTransporte.Veiculos.Items[I].Placa);
    Gerador.wGrupoNFSe('/Veiculos');
  end;

  Gerador.Prefixo := aPrefixo;
end;

function TCIOTW_eFrete.GerarXml: Boolean;
var
//  Prefixo, NameSpaceServico, NameSpaceBase: string;
  Ok: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';
  // Carrega Layout que sera utilizado para gera o txt
  Gerador.LayoutArquivoTXT.Clear;
  Gerador.ArquivoFormatoTXT := '';
  Gerador.Prefixo := 'pef:';

  VersaoDF := DblToVersaoCIOT(Ok, CIOT.OperacaoTransporte.Versao);
  versao := VersaoCIOTToInt(VersaoDF);

  case CIOT.Integradora.Operacao of
    opObterPdf:
      begin
//        Gerador.wGrupoNFSe('ObterOperacaoTransportePdfRequest ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//        Gerador.wCampoNFSe(tcStr, '', 'CodigoIdentificacaoOperacao', 01, 30, 1, CIOT.OperacaoTransporte.NumeroCIOT, '');
////        Gerador.wTexto('<DocumentoViagem ' +   NAME_SPACE_EFRETE_OBJECTS + '>'++'</DocumentoViagem >');
//        Gerador.wTexto('<Integrador ' +   NAME_SPACE_EFRETE_OBJECTS + '>' + CIOT.Integradora.HashIntegrador + '</Integrador>');
////        Gerador.wTexto('<Token ' +   NAME_SPACE_EFRETE_OBJECTS + '>'++'</Token>');
//        Gerador.wTexto('<Versao ' +   NAME_SPACE_EFRETE_OBJECTS + '>1</Versao>');
//        Gerador.wGrupoNFSe('/ObterOperacaoTransportePdfRequest');
      end;

    opAdicionar:
      begin
        Gerador.wGrupoNFSe('AdicionarOperacaoTransporte'{ + NAME_SPACE_BASE}, '');

        Gerador.Prefixo := 'obj:';
        Gerador.wGrupoNFSe('AdicionarOperacaoTransporteRequest' {+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE}, 'AP01');

        GerarIdentificacao(5);

        Gerador.wCampoNFSe(tcStr, 'AP02', 'TipoViagem', 01, 01, 1, TipoViagemCIOTToStr(CIOT.OperacaoTransporte.TipoViagem));
        Gerador.wCampoNFSe(tcBoolStr, 'AP06', 'EmissaoGratuita', 01, 01, 1, CIOT.OperacaoTransporte.EmissaoGratuita);

        if CIOT.OperacaoTransporte.TipoViagem <> Frota then
          Gerador.wCampoNFSe(tcStr, 'AP07', 'BloquearNaoEquiparado', 01, 01, 1, LowerCase(BoolToStr(CIOT.OperacaoTransporte.BloquearNaoEquiparado, True)));

        Gerador.wCampoNFSe(tcStr, 'AP08', 'MatrizCNPJ', 01, 14, 1, CIOT.OperacaoTransporte.MatrizCNPJ);
        Gerador.wCampoNFSe(tcStr, 'AP09', 'FilialCNPJ', 01, 01, 1, CIOT.OperacaoTransporte.FilialCNPJ);
        Gerador.wCampoNFSe(tcStr, 'AP10', 'IdOperacaoCliente', 01, 01, 0, CIOT.OperacaoTransporte.IdOperacaoCliente, 'Id / Chave primária da operação de transporte no sistema do Cliente.');

//        if CIOT.OperacaoTransporte.TipoViagem <> TAC_Agregado then //Se TipoViagem for TAC_Agregado o campo não deve ser preenchido.
          Gerador.wCampoNFSe(tcDat, 'AP11', 'DataInicioViagem', 01, 01, 1, CIOT.OperacaoTransporte.DataInicioViagem);
//        else
//          Gerador.wCampoNFSe(tcStr, 'AP11', 'DataInicioViagem', 01, 01, 1, '');

        Gerador.wCampoNFSe(tcDat, 'AP12', 'DataFimViagem', 01, 01, 1, CIOT.OperacaoTransporte.DataFimViagem, 'Data prevista para o fim de viagem.');

        case CIOT.OperacaoTransporte.TipoViagem of
          Padrao:
            Gerador.wCampoNFSe(tcInt, 'AP13', 'CodigoNCMNaturezaCarga', 01, 04, 1, CIOT.OperacaoTransporte.CodigoNCMNaturezaCarga);
//          TAC_Agregado:
//            Gerador.wCampoNFSe(tcStr, 'AP13', 'CodigoNCMNaturezaCarga', 01, 01, 0, '');
        end;

        if CIOT.OperacaoTransporte.TipoViagem <> Frota then
          Gerador.wCampoNFSe(tcDe4, 'AP14', 'PesoCarga', 01, 01, 1, CIOT.OperacaoTransporte.PesoCarga);

        Gerador.wCampoNFSe(tcStr, 'AP15', 'TipoEmbalagem', 01, 01, 0, TipoEmbalagemCIOTToStr(CIOT.OperacaoTransporte.TipoEmbalagem));

        if CIOT.OperacaoTransporte.TipoViagem = Padrao then
          GerarViagem;

        //Adiciona Impostos
        if CIOT.OperacaoTransporte.TipoViagem <> Frota then
          GerarImpostos;

        //Adiciona os Pagamentos
        GerarPagamentos;

        //Adiciona Contratado
        GerarContratado;

        //Adiciona Motorista
        GerarMotorista;

        //Adiciona Destinatario
        if CIOT.OperacaoTransporte.TipoViagem <> Frota then
          GerarDestinatario;

        //Adiciona Contratante
        GerarContratante;

        if CIOT.OperacaoTransporte.TipoViagem <> Frota then
        begin
          //Adiciona SubContratante
          GerarSubContratante;

          //Adiciona Consignatario
          GerarConsignatario;

          //Adiciona TomadorServico
          GerarTomadorServico;
        end;

        //Adiciona Veículos
        GerarVeiculos;

        //Informar um CIOT (se existente) que esteja relacionado à operação de transporte.
        //Por exemplo: No caso da presença de um Subcontratante na operação de transporte informar
        //o CIOT onde o Subcontratante foi o Contratado.
        Gerador.wCampoNFSe(tcStr, 'AP203', 'CodigoIdentificacaoOperacaoPrincipal', 01, 01, 0, CIOT.OperacaoTransporte.CodigoIdentificacaoOperacaoPrincipal);

        if CIOT.OperacaoTransporte.ObservacoesAoTransportador <> '' then
        begin
          Gerador.wGrupoNFSe('ObservacoesAoTransportador', 'AP204');
          Gerador.wCampoNFSe(tcStr, 'AP205', 'string', 01, 01, 1, CIOT.OperacaoTransporte.ObservacoesAoTransportador);
          Gerador.wGrupoNFSe('/ObservacoesAoTransportador');
        end;

        if CIOT.OperacaoTransporte.ObservacoesAoCredenciado <> '' then
        begin
          Gerador.wGrupoNFSe('ObservacoesAoCredenciado', 'AP206');
          Gerador.wCampoNFSe(tcStr, 'AP207', 'string', 01, 01, 1, CIOT.OperacaoTransporte.ObservacoesAoCredenciado);
          Gerador.wGrupoNFSe('/ObservacoesAoCredenciado');
        end;

        Gerador.wCampoNFSe(tcStr, 'AP208', 'EntregaDocumentacao', 01, 01, 1, EntregaDocumentacaoToStr(CIOT.OperacaoTransporte.EntregaDocumentacao));
        Gerador.wCampoNFSe(tcInt, 'AP209', 'QuantidadeSaques', 01, 01, 1, CIOT.OperacaoTransporte.QuantidadeSaques);
        Gerador.wCampoNFSe(tcInt, 'AP210', 'QuantidadeTransferencias', 01, 01, 1, CIOT.OperacaoTransporte.QuantidadeTransferencias);

        Gerador.wCampoNFSe(tcDe2, 'AP211', 'ValorSaques', 01, 01, 1, CIOT.OperacaoTransporte.ValorSaques);
        Gerador.wCampoNFSe(tcDe2, 'AP212', 'ValorTransferencias', 01, 01, 1, CIOT.OperacaoTransporte.ValorTransferencias);
        Gerador.wCampoNFSe(tcInt, 'AP213', 'CodigoTipoCarga', 01, 01, 1, CIOT.OperacaoTransporte.CodigoTipoCarga);

        Gerador.wGrupoNFSe('/AdicionarOperacaoTransporteRequest');

        Gerador.Prefixo := 'pef:';
        Gerador.wGrupoNFSe('/AdicionarOperacaoTransporte');
      end;

    opRetificar:
      begin
//        Gerador.wGrupoNFSe('RetificarOperacaoTransporteRequest ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'WP01');
//
//        Gerador.wCampoNFSe(tcStr, 'WP02', 'CodigoIdentificacaoOperacao', 01, 01, 1, FOperacaoTransporte.NumeroCIOT, '');
//        Gerador.wCampoNFSe(tcInt, 'WP03', 'CodigoMunicipioDestino', 01, 07, 1, FOperacaoTransporte.Viagens.Items[0].CodigoMunicipioDestino); //0001
//        Gerador.wCampoNFSe(tcInt, 'WP04', 'CodigoMunicipioOrigem', 01, 07, 1, FOperacaoTransporte.Viagens.Items[0].CodigoMunicipioOrigem); //0001
//        Gerador.wCampoNFSe(tcInt, 'WP05', 'CodigoNCMNaturezaCarga', 01, 04, 1, FOperacaoTransporte.CodigoNCMNaturezaCarga); //0001
//        Gerador.wCampoNFSe(tcDat, 'WP06', 'DataFimViagem', 01, 01, 1, FOperacaoTransporte.DataFimViagem); //0001
//        Gerador.wCampoNFSe(tcDat, 'WP07', 'DataInicioViagem', 01, 01, 1, FOperacaoTransporte.DataInicioViagem); //0001
//        Gerador.wCampoNFSe(tcStr, 'WP08', 'Integrador', 01, 01, 1, TAmsCIOT( FOperacaoTransporte.Owner ).Configuracoes.Integradora.Identificacao, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//        Gerador.wCampoNFSe(tcDe4, 'WP09', 'PesoCarga', 01, 01, 1, FOperacaoTransporte.PesoCarga); //0001
//        Gerador.wCampoNFSe(tcStr, 'WP10', 'Token', 01, 01, 1, '');
//
//        Gerador.wCampoNFSe(tcStr, 'WP13', 'Versao', 001, 001, 1, 1, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//
//        Gerador.wGrupoNFSe('Veiculos ' + NAME_SPACE_EFRETE_PEFRETIFICAR_OBJECTS, 'WP11');
//        for I := 0 to FOperacaoTransporte.Veiculos.Count -1 do
//        begin
//          with FOperacaoTransporte.Veiculos.Items[I] do
//            Gerador.wCampoNFSe(tcStr, 'WP12', 'Placa', 001, 001, 1, Placa, 'Placa do veículo conforme exemplo: AAA1234.');
//        end;
//        Gerador.wGrupoNFSe('/Veiculos');
//
//        Gerador.wGrupoNFSe('/RetificarOperacaoTransporteRequest');
      end;

    opCancelar:
      begin
        Gerador.wGrupoNFSe('CancelarOperacaoTransporteRequest', 'KP01');
        Gerador.wCampoNFSe(tcStr, 'KP02', 'CodigoIdentificacaoOperacao', 01, 01, 1, CIOT.OperacaoTransporte.NumeroCIOT);
        Gerador.wCampoNFSe(tcStr, 'KP03', 'Integrador', 01, 01, 1, CIOT.Integradora.Integrador);
        Gerador.wCampoNFSe(tcStr, 'KP04', 'Motivo', 01, 01, 1, CIOT.OperacaoTransporte.Cancelamento.Motivo, '');
//        Gerador.wCampoNFSe(tcStr, 'KP05', 'Token ', 01, 01, 1, '');
        Gerador.wCampoNFSe(tcInt, 'KP06', 'Versao', 01, 01, 1, 1);
        Gerador.wGrupoNFSe('/CancelarOperacaoTransporteRequest');
      end;

    opAdicionarViagem:
      begin
//        if FOperacaoTransporte.TipoViagem = TAC_Agregado then
//        begin
//          Gerador.wGrupoNFSe('AdicionarViagemRequest' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//          Gerador.wCampoNFSe(tcStr, '', 'Integrador', 001, 001, 1, TAmsCIOT( FOperacaoTransporte.Owner ).Configuracoes.Integradora.Identificacao, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//          Gerador.wCampoNFSe(tcStr, '', 'Versao', 001, 001, 1, 2, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//          Gerador.wCampoNFSe(tcStr, '', 'CodigoIdentificacaoOperacao', 001, 030, 1, FOperacaoTransporte.NumeroCIOT, '');
//          Gerador.wGrupoNFSe('Viagens ' + NAME_SPACE_EFRETE_PEFADICIONAR_VIAGEM, '');
//
//          for I := 0 to FOperacaoTransporte.Viagens.Count -1 do
//          begin
//            Gerador.wGrupoNFSe('Viagem');
//            with FOperacaoTransporte.Viagens.Items[I] do
//            begin
//              Gerador.wCampoNFSe(tcInt, 'AP133', 'CodigoMunicipioDestino', 001, 007, 1, CodigoMunicipioDestino);
//              Gerador.wCampoNFSe(tcInt, 'AP134', 'CodigoMunicipioOrigem', 001, 007, 1, CodigoMunicipioOrigem);
//              Gerador.wCampoNFSe(tcStr, 'AP135', 'DocumentoViagem', 001, 001, 1, DocumentoViagem, 'Exemplo: CT-e / Serie, CTRC / Serie, Ordem de Serviço.');
//
//              for J := 0 to NotasFiscais.Count -1 do
//              begin
//                with NotasFiscais.Items[J] do
//                begin
//                  Gerador.wGrupoNFSe('NotasFiscais');
//                  Gerador.wCampoNFSe(tcInt, 'AP137', 'CodigoNCMNaturezaCarga', 001, 004, 1, CodigoNCMNaturezaCarga);
//                  Gerador.wCampoNFSe(tcDat, 'AP138', 'Data', 001, 004, 1, Data);
//                  Gerador.wCampoNFSe(tcStr, 'AP139', 'DescricaoDaMercadoria', 001, 060, 1, DescricaoDaMercadoria, 'Descrição adicional ao código NCM.');
//                  Gerador.wCampoNFSe(tcStr, 'AP140', 'Numero', 001, 010, 1, Numero);
//                  Gerador.wCampoNFSe(tcDe3, 'AP141', 'QuantidadeDaMercadoriaNoEmbarque', 001, 010, 1, QuantidadeDaMercadoriaNoEmbarque);
//                  Gerador.wCampoNFSe(tcStr, 'AP142', 'Serie', 001, 001, 1, Serie);
//                  Gerador.wCampoNFSe(tcStr, 'AP143', 'TipoDeCalculo', 001, 001, 1, TpVgTipoCalculoToStr(TipoDeCalculo));
//                  Gerador.wGrupoNFSe('ToleranciaDePerdaDeMercadoria', 'AP144');
//                  Gerador.wCampoNFSe(tcStr, 'AP145', 'Tipo', 001, 001, 1, TpProporcaoToStr(ToleranciaDePerdaDeMercadoria.Tipo));
//                  Gerador.wCampoNFSe(tcDe2, 'AP146', 'Valor', 001, 001, 1, ToleranciaDePerdaDeMercadoria.Valor);
//                  Gerador.wGrupoNFSe('/ToleranciaDePerdaDeMercadoria');
//
//                  if DiferencaDeFrete.Tipo <> SemDiferenca then
//                  begin
//                    Gerador.wGrupoNFSe('DiferencaDeFrete', 'AP147');
//                    Gerador.wCampoNFSe(tcStr, 'AP148', 'Tipo', 001, 001, 1, TpDifFreteToStr(DiferencaDeFrete.Tipo));
//                    Gerador.wCampoNFSe(tcStr, 'AP149', 'Base', 001, 001, 1, TpDiferencaFreteBCToStr(DiferencaDeFrete.Base));
//                    Gerador.wGrupoNFSe('Tolerancia', 'AP150');
//                    Gerador.wCampoNFSe(tcStr, 'AP151', 'Tipo', 001, 001, 1, TpProporcaoToStr(DiferencaDeFrete.Tolerancia.Tipo));
//                    Gerador.wCampoNFSe(tcDe2, 'AP152', 'Valor', 001, 001, 1, DiferencaDeFrete.Tolerancia.Valor);
//                    Gerador.wGrupoNFSe('/Tolerancia');
//                    Gerador.wGrupoNFSe('MargemGanho', 'AP153');
//                    Gerador.wCampoNFSe(tcStr, 'AP154', 'Tipo', 001, 001, 1, TpProporcaoToStr(DiferencaDeFrete.MargemGanho.Tipo));
//                    Gerador.wCampoNFSe(tcDe2, 'AP155', 'Valor', 001, 001, 1, DiferencaDeFrete.MargemGanho.Valor);
//                    Gerador.wGrupoNFSe('/MargemGanho');
//                    Gerador.wGrupoNFSe('MargemPerda', 'AP156');
//                    Gerador.wCampoNFSe(tcStr, 'AP157', 'Tipo', 001, 001, 1, TpProporcaoToStr(DiferencaDeFrete.MargemPerda.Tipo));
//                    Gerador.wCampoNFSe(tcDe2, 'AP158', 'Valor', 001, 001, 1, DiferencaDeFrete.MargemPerda.Valor);
//                    Gerador.wGrupoNFSe('/MargemPerda');
//                    Gerador.wGrupoNFSe('/DiferencaDeFrete');
//                  end;
//
//                  Gerador.wCampoNFSe(tcStr, 'AP159', 'UnidadeDeMedidaDaMercadoria', 001, 001, 1, TpUnMedMercToStr(UnidadeDeMedidaDaMercadoria));
//                  Gerador.wCampoNFSe(tcDe2, 'AP159', 'ValorDaMercadoriaPorUnidade', 001, 001, 1, ValorDaMercadoriaPorUnidade);
//                  Gerador.wCampoNFSe(tcDe2, 'AP159', 'ValorDoFretePorUnidadeDeMercadoria', 001, 001, 1, ValorDoFretePorUnidadeDeMercadoria);
//                  Gerador.wCampoNFSe(tcDe2, 'AP159', 'ValorTotal', 001, 001, 1, ValorTotal);
//
//                  Gerador.wGrupoNFSe('/NotasFiscais');
//                end;
//              end;
//
//              Gerador.wGrupoNFSe('Valores ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP163');
//              with Valores do
//              begin
//                Gerador.wCampoNFSe(tcDe2, 'AP164', 'Combustivel', 001, 001, 1, Combustivel);
//                Gerador.wCampoNFSe(tcStr, 'AP165', 'JustificativaOutrosCreditos', 001, 001, 1, JustificativaOutrosCreditos);
//                Gerador.wCampoNFSe(tcStr, 'AP166', 'JustificativaOutrosDebitos', 001, 001, 1, JustificativaOutrosDebitos);
//                Gerador.wCampoNFSe(tcDe2, 'AP167', 'OutrosCreditos', 001, 001, 1, OutrosCreditos);
//                Gerador.wCampoNFSe(tcDe2, 'AP168', 'OutrosDebitos', 001, 001, 1, OutrosDebitos);
//                Gerador.wCampoNFSe(tcDe2, 'AP169', 'Pedagio', 001, 001, 1, Pedagio);
//                Gerador.wCampoNFSe(tcDe2, 'AP170', 'Seguro', 001, 001, 1, Seguro);
//                Gerador.wCampoNFSe(tcDe2, 'AP171', 'TotalDeAdiantamento', 001, 001, 1, TotalDeAdiantamento);
//                Gerador.wCampoNFSe(tcDe2, 'AP172', 'TotalDeQuitacao', 001, 001, 1, TotalDeQuitacao);
//                Gerador.wCampoNFSe(tcDe2, 'AP173', 'TotalOperacao', 001, 001, 1, TotalOperacao);
//                Gerador.wCampoNFSe(tcDe2, 'AP174', 'TotalViagem', 001, 001, 1, TotalViagem);
//              end;
//              Gerador.wGrupoNFSe('/Valores');
//            end;
//
//            Gerador.wGrupoNFSe('/Viagem');
//          end;
//
//          Gerador.wGrupoNFSe('/Viagens');
//
//          Gerador.wGrupoNFSe('Pagamentos ' + NAME_SPACE_EFRETE_PEFADICIONAR_VIAGEM, '');
//
//          for I := 0 to FOperacaoTransporte.Pagamentos.Count -1 do
//          begin
//            with FOperacaoTransporte.Pagamentos.Items[I] do
//            begin
//              Gerador.wGrupoNFSe('Pagamento');
//              Gerador.wCampoNFSe(tcStr, 'AP92', 'Categoria', 001, 001, 1, TpCatPagToStr(Categoria), 'Categoria relacionada ao pagamento realizado. Restrita aos membros da ENUM: -Adiantamento, -Estadia, Quitacao, -SemCategoria ', ' ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//              Gerador.wCampoNFSe(tcDat, 'AP93', 'DataDeLiberacao', 001, 001, 1, DataDeLiberacao);
//              Gerador.wCampoNFSe(tcStr, 'AP94', 'Documento', 001, 020, 1, Documento, 'Documento relacionado a viagem.');
//              Gerador.wCampoNFSe(tcStr, 'AP94', 'IdPagamentoCliente', 001, 020, 1, IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente. ');
//              Gerador.wCampoNFSe(tcStr, 'AP95', 'InformacaoAdicional', 001, 000, 0, InformacaoAdicional, '');
//
//              Gerador.wGrupoNFSe('InformacoesBancarias ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP97');
//              with InformacoesBancarias do
//              begin
//                Gerador.wCampoNFSe(tcStr, 'AP98', 'Agencia', 001, 001, 1, Agencia);
//                Gerador.wCampoNFSe(tcStr, 'AP99', 'Conta', 001, 001, 1, Conta);
//                Gerador.wCampoNFSe(tcStr, 'AP100', 'InstituicaoBancaria', 001, 001, 1, InstituicaoBancaria);
//              end;
//              Gerador.wGrupoNFSe('/InformacoesBancarias');
//
//              Gerador.wCampoNFSe(tcStr, 'AP101', 'TipoPagamento', 001, 020, 1, TpPagamentoToStr(TipoPagamento), 'Tipo de pagamento que será usado pelo contratante. Restrito aos itens da enum: -TransferenciaBancaria -eFRETE', ' ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//              Gerador.wCampoNFSe(tcDe2, 'AP102', 'Valor', 001, 020, 1, Valor, 'Valor do pagamento.');
//              Gerador.wGrupoNFSe('/Pagamento');
//            end;
//          end;
//
//          Gerador.wGrupoNFSe('/Pagamentos');
//
//          Gerador.wCampoNFSe(tcStr, '', 'NaoAdicionarParcialmente', 001, 001, 1, 'false', '');
//          Gerador.wGrupoNFSe('/AdicionarViagemRequest');
//        end;
      end;

    opAdicionarPagamento:
      begin
//        if FOperacaoTransporte.TipoViagem = TAC_Agregado then
//        begin
//          Gerador.wGrupoNFSe('AdicionarPagamentoRequest ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//          Gerador.wCampoNFSe(tcStr, '', 'Integrador', 001, 001, 1, TAmsCIOT( FOperacaoTransporte.Owner ).Configuracoes.Integradora.Identificacao, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//          Gerador.wCampoNFSe(tcStr, '', 'Versao', 001, 001, 1, 2, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//          Gerador.wCampoNFSe(tcStr, '', 'CodigoIdentificacaoOperacao', 001, 030, 1, FOperacaoTransporte.NumeroCIOT, '');
//          Gerador.wGrupoNFSe('Pagamentos ' + NAME_SPACE_EFRETE_PEFADICIONAR_PAGAMENTOS, '');
//
//          for I := 0 to FOperacaoTransporte.Pagamentos.Count -1 do
//          begin
//            with FOperacaoTransporte.Pagamentos.Items[I] do
//            begin
//              Gerador.wGrupoNFSe('Pagamento');
//              Gerador.wCampoNFSe(tcStr, 'AP92', 'Categoria', 001, 001, 1, TpCatPagToStr(Categoria), 'Categoria relacionada ao pagamento realizado. Restrita aos membros da ENUM: -Adiantamento, -Estadia, Quitacao, -SemCategoria ', ' ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//              Gerador.wCampoNFSe(tcDat, 'AP93', 'DataDeLiberacao', 001, 001, 1, DataDeLiberacao);
//              Gerador.wCampoNFSe(tcStr, 'AP94', 'Documento', 001, 020, 1, Documento, 'Documento relacionado a viagem.');
//              Gerador.wCampoNFSe(tcStr, 'AP94', 'IdPagamentoCliente', 001, 020, 1, IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente. ');
//              Gerador.wCampoNFSe(tcStr, 'AP95', 'InformacaoAdicional', 001, 000, 0, InformacaoAdicional, '');
//
//              Gerador.wGrupoNFSe('InformacoesBancarias ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP97');
//              with InformacoesBancarias do
//              begin
//                Gerador.wCampoNFSe(tcStr, 'AP98', 'Agencia', 001, 001, 1, Agencia);
//                Gerador.wCampoNFSe(tcStr, 'AP99', 'Conta', 001, 001, 1, Conta);
//                Gerador.wCampoNFSe(tcStr, 'AP100', 'InstituicaoBancaria', 001, 001, 1, InstituicaoBancaria);
//              end;
//              Gerador.wGrupoNFSe('/InformacoesBancarias');
//
//              Gerador.wCampoNFSe(tcStr, 'AP101', 'TipoPagamento', 001, 020, 1, TpPagamentoToStr(TipoPagamento), 'Tipo de pagamento que será usado pelo contratante. Restrito aos itens da enum: -TransferenciaBancaria -eFRETE', ' ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//              Gerador.wCampoNFSe(tcDe2, 'AP102', 'Valor', 001, 020, 1, Valor, 'Valor do pagamento.');
//              Gerador.wGrupoNFSe('/Pagamento');
//            end;
//          end;
//
//          Gerador.wGrupoNFSe('/Pagamentos');
//          Gerador.wGrupoNFSe('/AdicionarPagamentoRequest');
//        end;
      end;

    opCancelarPagamento:
      begin
//        if FOperacaoTransporte.TipoViagem = TAC_Agregado then
//        begin
//          Gerador.wGrupoNFSe('CancelarPagamentoRequest ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//          Gerador.wCampoNFSe(tcStr, '', 'Integrador', 001, 001, 1, TAmsCIOT( FOperacaoTransporte.Owner ).Configuracoes.Integradora.Identificacao, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//          Gerador.wCampoNFSe(tcStr, '', 'Versao', 001, 001, 1, 1, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//          Gerador.wCampoNFSe(tcStr, '', 'CodigoIdentificacaoOperacao', 001, 030, 1, FOperacaoTransporte.NumeroCIOT, '');
//          Gerador.wCampoNFSe(tcStr, '', 'IdPagamentoCliente', 001, 020, 1, FOperacaoTransporte.Cancelamento.IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente. ');
//          Gerador.wCampoNFSe(tcStr, 'KP04', 'Motivo', 001, 001, 1, FOperacaoTransporte.Cancelamento.Motivo, '');
//          Gerador.wGrupoNFSe('/CancelarPagamentoRequest');
//        end;
      end;

    opEncerrar:
      begin
        Gerador.wGrupoNFSe('EncerrarOperacaoTransporteRequest', 'QP01');

        Gerador.wCampoNFSe(tcStr, 'QP02', 'CodigoIdentificacaoOperacao', 01, 01, 1, CIOT.OperacaoTransporte.NumeroCIOT);
        Gerador.wCampoNFSe(tcDe6, 'QP03', 'PesoCarga', 01, 01, 1, CIOT.OperacaoTransporte.PesoCarga, 'Peso da carga que foi transportado.');

        Gerador.wGrupoNFSe('Impostos', 'QP04');
        Gerador.wCampoNFSe(tcStr, 'QP05', 'DescricaoOutrosImpostos', 01, 01, 1, CIOT.OperacaoTransporte.Impostos.DescricaoOutrosImpostos);
        Gerador.wCampoNFSe(tcDe2, 'QP06', 'INSS', 01, 20, 1, CIOT.OperacaoTransporte.Impostos.INSS, 'Valor destinado ao INSS. Este valor deverá fazer parte do valor de Adiantamento ou do valor de Quitação.');
        Gerador.wCampoNFSe(tcDe2, 'QP07', 'IRRF', 01, 20, 1, CIOT.OperacaoTransporte.Impostos.IRRF, 'Valor destinado ao IRRF. Este valor deverá fazer parte do valor de Adiantamento ou do valor de Quitação.');
        Gerador.wCampoNFSe(tcDe2, 'QP08', 'ISSQN', 01, 20, 1, CIOT.OperacaoTransporte.Impostos.ISSQN, 'Valor destinado ao ISSQN. Este valor deverá fazer parte do valor de Adiantamento ou do valor de Quitação.');
        Gerador.wCampoNFSe(tcDe2, 'QP09', 'OutrosImpostos', 01, 20, 1, CIOT.OperacaoTransporte.Impostos.OutrosImpostos, 'Valor destinado a outros impostos não previstos. Este valor deverá fazer parte do valor de Adiantamento ou do valor de Quitação.');
        Gerador.wCampoNFSe(tcDe2, 'QP10', 'SestSenat', 01, 20, 1, CIOT.OperacaoTransporte.Impostos.SestSenat, 'Valor destinado ao SEST / SENAT. Este valor deverá fazer parte do valor de Adiantamento ou do valor de Quitação.');
        Gerador.wGrupoNFSe('/Impostos');

        Gerador.wCampoNFSe(tcStr, 'QP11', 'Integrador', 01, 01, 1, CIOT.Integradora.Integrador);
//        Gerador.wCampoNFSe(tcStr, 'QP12', 'Token', 01, 01, 1, '');
        Gerador.wCampoNFSe(tcInt, 'QP13', 'Versao', 01, 01, 1, 1);
        Gerador.wCampoNFSe(tcStr, 'QP14', 'QuantidadeSaques', 01, 01, 1, CIOT.OperacaoTransporte.QuantidadeSaques);
        Gerador.wCampoNFSe(tcStr, 'QP15', 'QuantidadeTransferencia', 01, 01, 1, CIOT.OperacaoTransporte.QuantidadeTransferencias);

        Gerador.wGrupoNFSe('/EncerrarOperacaoTransporteRequest');
      end;
  end;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
