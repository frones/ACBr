package br.com.acbr.lib.comum.dfe;

import java.util.HashMap;
import java.util.Map;

public class cClassTribIBSCBS {

    private static final Map<String, cClassTribIBSCBS> lookup = new HashMap<>();

    public static final cClassTribIBSCBS cctNenhum = new cClassTribIBSCBS("", "Nenhum");
    public static final cClassTribIBSCBS cct000001 = new cClassTribIBSCBS("000001", "Situações tributadas integralmente pelo IBS e CBS");
    public static final cClassTribIBSCBS cct000002 = new cClassTribIBSCBS("000002", "Exploração de via, observado o art. 11 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct000003 = new cClassTribIBSCBS("000003", "Regime automotivo - projetos incentivados, observado o art. 311 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct000004 = new cClassTribIBSCBS("000004", "Regime automotivo - projetos incentivados, observado o art. 312 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct010001 = new cClassTribIBSCBS("010001", "Operações do FGTS não realizadas pela Caixa Econômica Federal, observado o art. 212 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct011001 = new cClassTribIBSCBS("011001", "Planos de assistência funerária, observado o art. 236 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct011002 = new cClassTribIBSCBS("011002", "Planos de assistência à saúde, observado o art. 237 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct011003 = new cClassTribIBSCBS("011003", "Intermediação de planos de assistência à saúde, observado o art. 240 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct011004 = new cClassTribIBSCBS("011004", "Concursos e prognósticos, observado o art. 246 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct011005 = new cClassTribIBSCBS("011005", "Planos de assistência à saúde de animais domésticos, observado o art. 243 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200001 = new cClassTribIBSCBS("200001", "Aquisições de máquinas, de aparelhos, de instrumentos, de equipamentos, de matérias-primas, de produtos intermediários e de materiais de embalagem realizadas entre empresas autorizadas a operar em zonas de processamento de exportação, observado o art. 103 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200002 = new cClassTribIBSCBS("200002", "Fornecimento ou importação de tratores, máquinas e implementos agrícolas, destinados a produtor rural não contribuinte, e de veículos de transporte de carga destinados a transportador autônomo de carga pessoa física não contribuinte, observado o art. 110 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200003 = new cClassTribIBSCBS("200003", "Vendas de produtos destinados à alimentação humana relacionados no Anexo I da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, que compõem a Cesta Básica Nacional de Alimentos, criada nos termos do art. 8º da Emenda Constitucional nº 132, de 20 de dezembro de 2023, observado o art. 125 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200004 = new cClassTribIBSCBS("200004", "Venda de dispositivos médicos com a especificação das respectivas classificações da NCM/SH previstas no Anexo XII da Lei Complementar nº 214, de 2025, observado o art. 144 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200005 = new cClassTribIBSCBS("200005", "Venda de dispositivos médicos com a especificação das respectivas classificações da NCM/SH previstas no Anexo IV da Lei Complementar nº 214, de 2025, quando adquiridos por órgãos da administração pública direta, autarquias e fundações públicas, observado o art. 144 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200006 = new cClassTribIBSCBS("200006", "Situação de emergência de saúde pública reconhecida pelo Poder Legislativo federal, estadual, distrital ou municipal competente, ato conjunto do Ministro da Fazenda e do Comitê Gestor do IBS poderá ser editado, a qualquer momento, para incluir dispositivos não listados no Anexo XIII da Lei Complementar nº 214, de 2025, limitada a vigência do benefício ao período e à localidade da emergência de saúde pública, observado o art. 144 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200007 = new cClassTribIBSCBS("200007", "Fornecimento dos dispositivos de acessibilidade próprios para pessoas com deficiência relacionados no Anexo XIV da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 145 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200008 = new cClassTribIBSCBS("200008", "Fornecimento dos dispositivos de acessibilidade próprios para pessoas com deficiência relacionados no Anexo V da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, quando adquiridos por órgãos da administração pública direta, autarquias, fundações públicas e entidades imunes, observado o art. 145 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200009 = new cClassTribIBSCBS("200009", "Fornecimento dos medicamentos relacionados no Anexo XIV da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 146 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200010 = new cClassTribIBSCBS("200010", "Fornecimento dos medicamentos registrados na Anvisa, quando adquiridos por órgãos da administração pública direta, autarquias, fundações públicas e entidades imunes, observado o art. 146 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200011 = new cClassTribIBSCBS("200011", "Fornecimento das composições para nutrição enteral e parenteral, composições especiais e fórmulas nutricionais destinadas às pessoas com erros inatos do metabolismo relacionadas no Anexo VI da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, quando adquiridas por órgãos da administração pública direta, autarquias e fundações públicas, observado o art. 146 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200012 = new cClassTribIBSCBS("200012", "Situação de emergência de saúde pública reconhecida pelo Poder público (Anexo XIV)");
    public static final cClassTribIBSCBS cct200013 = new cClassTribIBSCBS("200013", "Fornecimento de tampões higiênicos, absorventes higiênicos internos ou externos, descartáveis ou reutilizáveis, calcinhas absorventes e coletores menstruais, observado o art. 147 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200014 = new cClassTribIBSCBS("200014", "Fornecimento dos produtos hortícolas, frutas e ovos, relacionados no Anexo XV da Lei Complementar nº 214 , de 2025, com a especificação das respectivas classificações da NCM/SH e desde que não cozidos, observado o art. 148 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200015 = new cClassTribIBSCBS("200015", "Venda de automóveis de passageiros de fabricação nacional de, no mínimo, 4 (quatro) portas, inclusive a de acesso ao bagageiro, quando adquiridos por motoristas profissionais que exerçam, comprovadamente, em automóvel de sua propriedade, atividade de condutor autônomo de passageiros, na condição de titular de autorização, permissão ou concessão do poder público, e que destinem o automóvel à utilização na categoria de aluguel (táxi), ou por pessoas com deficiência física, visual, auditiva, deficiência mental severa ou profunda, transtorno do espectro autista, com prejuízos na \\r\\n comunicação social e em padrões restritos ou repetitivos de comportamento de nível moderado ou grave, nos termos da legislação relativa à matéria, observado o disposto no art. 149 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200016 = new cClassTribIBSCBS("200016", "Prestação de serviços de pesquisa e desenvolvimento por Instituição Científica, Tecnológica e de Inovação (ICT) sem fins lucrativos para a administração pública direta, autarquias e fundações públicas ou para o contribuinte sujeito ao regime regular do IBS e da CBS, observado o disposto no art. 156  da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200017 = new cClassTribIBSCBS("200017", "Operações relacionadas ao FGTS, considerando aquelas necessárias à aplicação da Lei nº 8.036, de 1990, realizadas pelo Conselho Curador ou Secretaria Executiva do FGTS, observado o art. 212 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200018 = new cClassTribIBSCBS("200018", "Operações de resseguro e retrocessão ficam sujeitas à incidência à alíquota zero, inclusive quando os prêmios de resseguro e retrocessão forem cedidos ao exterior, observado o art. 223 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200019 = new cClassTribIBSCBS("200019", "Importador dos serviços financeiros seja contribuinte que realize as operações de que tratam os incisos I a V do caput do art. 182, será aplicada alíquota zero na importação, sem prejuízo da manutenção do direito de dedução dessas despesas da base de cálculo do IBS e da CBS, segundo, observado o art. 231 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200020 = new cClassTribIBSCBS("200020", "Operação praticada por sociedades cooperativas optantes por regime específico do IBS e CBS, quando o associado destinar bem ou serviço à cooperativa de que participa, e a cooperativa fornecer bem ou serviço ao associado sujeito ao regime regular do IBS e da CBS, observado o art. 271 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200021 = new cClassTribIBSCBS("200021", "Serviços de transporte público coletivo de passageiros ferroviário e hidroviário urbanos, semiurbanos e metropolitanos, observado o art. 285 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200022 = new cClassTribIBSCBS("200022", "Operação originada fora da Zona Franca de Manaus que destine bem material industrializado de origem nacional a contribuinte estabelecido na Zona Franca de Manaus que seja habilitado nos termos do art. 442 da Lei Complementar nº 214, de 2025, e sujeito ao regime regular do IBS e da CBS ou optante pelo regime do Simples Nacional de que trata o art. 12 da Lei Complementar nº 123, de 2006, observado o art. 445 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200023 = new cClassTribIBSCBS("200023", "Operação realizada por indústria incentivada que destine bem material intermediário para outra indústria incentivada na Zona Franca de Manaus, desde que a entrega ou disponibilização dos bens ocorra dentro da referida área, observado o art. 448 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200024 = new cClassTribIBSCBS("200024", "Operação originada fora das Áreas de Livre Comércio que destine bem material industrializado de origem nacional a contribuinte estabelecido nas Áreas de Livre Comércio que seja habilitado nos termos do art. 456 da Lei Complementar nº 214, de 2025, e sujeito ao regime regular do IBS e da CBS ou optante pelo regime do Simples Nacional de que trata o art. 12 da Lei Complementar nº 123, de 2006, observado o art. 463 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200025 = new cClassTribIBSCBS("200025", "Fornecimento dos serviços de educação relacionados ao Programa Universidade para Todos (Prouni), instituído pela Lei nº 11.096, de 13 de janeiro de 2005, observado o art. 308 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200026 = new cClassTribIBSCBS("200026", "Locação de imóveis localizados nas zonas reabilitadas, pelo prazo de 5 (cinco) anos, contado da data de expedição do habite-se, e relacionados a projetos de reabilitação urbana de zonas históricas e de áreas críticas de recuperação e reconversão urbanística dos Municípios ou do Distrito Federal, a serem delimitadas por lei municipal ou distrital, observado o art. 158 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200027 = new cClassTribIBSCBS("200027", "Operações de locação, cessão onerosa e arrendamento de bens imóveis, observado o art. 261 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200028 = new cClassTribIBSCBS("200028", "Fornecimento dos serviços de educação relacionados no Anexo II da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da Nomenclatura Brasileira de Serviços, Intangíveis e Outras Operações que Produzam Variações no Patrimônio (NBS), observado o art. 129 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200029 = new cClassTribIBSCBS("200029", "Fornecimento dos serviços de saúde humana relacionados no Anexo III da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NBS, observado o art. 130 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200030 = new cClassTribIBSCBS("200030", "Venda dos dispositivos médicos relacionados no Anexo IV da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 131 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200031 = new cClassTribIBSCBS("200031", "Fornecimento dos dispositivos de acessibilidade próprios para pessoas com deficiência relacionados no Anexo V da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 132 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200032 = new cClassTribIBSCBS("200032", "Fornecimento dos medicamentos registrados na Anvisa ou produzidos por farmácias de manipulação, ressalvados os medicamentos sujeitos à alíquota zero de que trata o art. 141 da Lei Complementar nº 214, de 2025, observado o art. 133 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200033 = new cClassTribIBSCBS("200033", "Fornecimento das composições para nutrição enteral e parenteral, composições especiais e fórmulas nutricionais destinadas às pessoas com erros inatos do metabolismo relacionadas no Anexo VI da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 133 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200034 = new cClassTribIBSCBS("200034", "Fornecimento dos alimentos destinados ao consumo humano relacionados no Anexo VII da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 135 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200035 = new cClassTribIBSCBS("200035", "Fornecimento dos produtos de higiene pessoal e limpeza relacionados no Anexo VIII da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 136 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200036 = new cClassTribIBSCBS("200036", "Fornecimento de produtos agropecuários, aquícolas, pesqueiros, florestais e extrativistas vegetais in natura, observado o art. 137 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200037 = new cClassTribIBSCBS("200037", "Fornecimento de serviços ambientais de conservação ou recuperação da vegetação nativa, mesmo que fornecidos sob a forma de manejo sustentável de sistemas agrícolas, agroflorestais e agrossilvopastoris, em conformidade com as definições e requisitos da legislação específica, observado o art. 137 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200038 = new cClassTribIBSCBS("200038", "Fornecimento dos insumos agropecuários e aquícolas relacionados no Anexo IX da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH e da NBS, observado o art. 138 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200039 = new cClassTribIBSCBS("200039", "Fornecimento dos serviços e o licenciamento ou cessão dos direitos relacionados no Anexo X da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NBS, quando destinados às seguintes produções nacionais artísticas, culturais, de eventos, jornalísticas e audiovisuais: espetáculos teatrais, circenses e de dança, shows musicais, desfiles carnavalescos ou folclóricos, eventos acadêmicos e científicos, como congressos, conferências e simpósios, feiras de negócios, exposições, feiras e mostras culturais, artísticas e literárias; programas de auditório ou jornalísticos, filmes, documentários, séries, novelas, entrevistas e clipes musicais, observado o art. 139 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200040 = new cClassTribIBSCBS("200040", "Fornecimento dos seguintes serviços de comunicação institucional à administração pública direta, autarquias e fundações públicas: serviços direcionados ao planejamento, criação, programação e manutenção de páginas eletrônicas da administração pública, ao monitoramento e gestão de suas redes sociais e à otimização de páginas e canais digitais para mecanismos de buscas e produção de mensagens, infográficos, painéis interativos e conteúdo institucional, serviços de relações com a imprensa, que reúnem estratégias organizacionais para promover e reforçar a comunicação dos órgãos e das entidades contratantes com seus públicos de interesse, por meio da interação com profissionais da imprensa, e serviços de relações públicas, que compreendem o esforço de comunicação planejado, coeso e contínuo que tem por objetivo estabelecer adequada percepção da atuação e dos objetivos institucionais, a partir do estímulo à compreensão mútua e da manutenção de padrões de relacionamento e fluxos de informação entre os órgãos e as entidades contratantes e seus públicos de interesse, no País e no exterior, observado o art. 140 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200041 = new cClassTribIBSCBS("200041", "Operações relacionadas às seguintes atividades desportivas: fornecimento de serviço de educação desportiva, classificado no código 1.2205.12.00 da NBS, e gestão e exploração do desporto por associações e clubes esportivos filiados ao órgão estadual ou federal responsável pela coordenação dos desportos, inclusive por meio de venda de ingressos para eventos desportivos, fornecimento oneroso ou não de bens e serviços, inclusive ingressos, por meio de programas de sócio-torcedor, cessão dos direitos desportivos dos atletas e transferência de atletas para outra entidade desportiva ou seu retorno à atividade em outra entidade desportiva, observado o art. 141 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200042 = new cClassTribIBSCBS("200042", "Operações relacionadas ao fornecimento de serviço de educação desportiva, classificado no código 1.2205.12.00 da NBS, observado o art. 141 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200043 = new cClassTribIBSCBS("200043", "Fornecimento à administração pública direta, autarquias e fundações púbicas dos serviços e dos bens relativos à soberania e à segurança nacional, à segurança da informação e à segurança cibernética relacionados no Anexo XI da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NBS e da NCM/SH, observado o art. 142 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200044 = new cClassTribIBSCBS("200044", "Operações e prestações de serviços de segurança da informação e segurança cibernética desenvolvidos por sociedade que tenha sócio brasileiro com o mínimo de 20% (vinte por cento) do seu capital social, relacionados no Anexo XI da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NBS e da NCM/SH, observado o art. 142 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200045 = new cClassTribIBSCBS("200045", "Operações relacionadas a projetos de reabilitação urbana de zonas históricas e de áreas críticas de recuperação e reconversão urbanística dos Municípios ou do Distrito Federal, a serem delimitadas por lei municipal ou distrital, observado o art. 158 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200046 = new cClassTribIBSCBS("200046", "Operações com bens imóveis, observado o art. 261 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200047 = new cClassTribIBSCBS("200047", "Bares e Restaurantes, observado o art. 275 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200048 = new cClassTribIBSCBS("200048", "Hotelaria, Parques de Diversão e Parques Temáticos, observado o art. 281 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200049 = new cClassTribIBSCBS("200049", "Transporte coletivo de passageiros rodoviário, ferroviário e hidroviário intermunicipais e interestaduais, observado o art. 286 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200051 = new cClassTribIBSCBS("200051", "Agências de Turismo, observado o art. 289 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200052 = new cClassTribIBSCBS("200052", "Prestação de serviços das seguintes profissões intelectuais de natureza científica, literária ou artística, submetidas à fiscalização por conselho profissional: administradores, advogados, arquitetos e urbanistas, assistentes sociais, bibliotecários, biólogos, contabilistas, economistas, economistas domésticos, profissionais de educação física, engenheiros e agrônomos, estatísticos, médicos veterinários e zootecnistas, museólogos, químicos, profissionais de relações públicas, técnicos industriais e técnicos agrícolas, observado o art. 127 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct200450 = new cClassTribIBSCBS("200450", "Serviços de transporte aéreo regional coletivo de passageiros ou de carga, observado o art. 287 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct210001 = new cClassTribIBSCBS("210001", "Redutor social aplicado uma única vez na alienação de bem imóvel residencial novo, observado o art. 259 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct210002 = new cClassTribIBSCBS("210002", "Redutor social aplicado uma única vez na alienação de lote residencial, observado o art. 259 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct210003 = new cClassTribIBSCBS("210003", "Redutor social em operações de locação, cessão onerosa e arrendamento de bens imóveis de uso residencial, observado o art. 260 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct220001 = new cClassTribIBSCBS("220001", "Incorporação imobiliária submetida ao regime especial de tributação, observado o art. 485 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct220002 = new cClassTribIBSCBS("220002", "Incorporação imobiliária submetida ao regime especial de tributação, observado o art. 485 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct220003 = new cClassTribIBSCBS("220003", "Alienação de imóvel decorrente de parcelamento do solo, observado o art. 486 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct221001 = new cClassTribIBSCBS("221001", "Locação, cessão onerosa ou arrendamento de bem imóvel com alíquota sobre a receita bruta, observado o art. 487 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct222001 = new cClassTribIBSCBS("222001", "Transporte internacional de passageiros, caso os trechos de ida e volta sejam vendidos em conjunto");
    public static final cClassTribIBSCBS cct400001 = new cClassTribIBSCBS("400001", "Fornecimento de serviços de transporte público coletivo de passageiros rodoviário e metroviário de caráter urbano, semiurbano e metropolitano, sob regime de autorização, permissão ou concessão pública, observado o art. 157 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410001 = new cClassTribIBSCBS("410001", "Fornecimento de bonificações quando constem do respectivo documento fiscal e que não dependam de evento posterior, observado o art. 5º da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410002 = new cClassTribIBSCBS("410002", "Transferências entre estabelecimentos pertencentes ao mesmo contribuinte, observado o art. 6º da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410003 = new cClassTribIBSCBS("410003", "Doações, observado o art. 6º da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410004 = new cClassTribIBSCBS("410004", "Exportações de bens e serviços, observado o art. 8º da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410005 = new cClassTribIBSCBS("410005", "Fornecimentos realizados pela União, pelos Estados, pelo Distrito Federal e pelos Municípios, observado o art. 9º da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410006 = new cClassTribIBSCBS("410006", "Fornecimentos realizados por entidades religiosas e templos de qualquer culto, inclusive suas organizações assistenciais e beneficentes, observado o art. 9º da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410007 = new cClassTribIBSCBS("410007", "Fornecimentos realizados por partidos políticos, inclusive suas fundações, entidades sindicais dos trabalhadores e instituições de educação e de assistência social, sem fins lucrativos, observado o art. 9º da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410008 = new cClassTribIBSCBS("410008", "Fornecimentos de livros, jornais, periódicos e do papel destinado a sua impressão, observado o art. 9º da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410009 = new cClassTribIBSCBS("410009", "Fornecimentos de fonogramas e videofonogramas musicais produzidos no Brasil contendo obras musicais ou literomusicais de autores brasileiros e/ou obras em geral interpretadas por artistas brasileiros, bem como os suportes materiais ou arquivos digitais que os contenham, salvo na etapa de replicação industrial de mídias ópticas de leitura a laser, observado o art. 9º da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410010 = new cClassTribIBSCBS("410010", "Fornecimentos de serviço de comunicação nas modalidades de radiodifusão sonora e de sons e imagens de recepção livre e gratuita, observado o art. 9º da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410011 = new cClassTribIBSCBS("410011", "Fornecimentos de ouro, quando definido em lei como ativo financeiro ou instrumento cambial, observado o art. 9º da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410012 = new cClassTribIBSCBS("410012", "Fornecimento de condomínio edilício não optante pelo regime regular, observado o art. 26 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410013 = new cClassTribIBSCBS("410013", "Exportações de combustíveis, observado o art. 98 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410014 = new cClassTribIBSCBS("410014", "Fornecimento de produtor rural não contribuinte, observado o art. 164 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410015 = new cClassTribIBSCBS("410015", "Fornecimento por transportador autônomo não contribuinte, observado o art. 169 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410016 = new cClassTribIBSCBS("410016", "Fornecimento ou aquisição de resíduos sólidos, observado o art. 170 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410017 = new cClassTribIBSCBS("410017", "Aquisição de bem móvel com crédito presumido sob condição de revenda realizada, observado o art. 171 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410018 = new cClassTribIBSCBS("410018", "Operações relacionadas aos fundos garantidores e executores de políticas públicas, inclusive de habitação, previstos em lei, assim entendidas os serviços prestados ao fundo pelo seu agente operador e por entidade encarregada da sua administração, observado o art. 213 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410019 = new cClassTribIBSCBS("410019", "Exclusão da gorjeta na base de cálculo no fornecimento de alimentação, observado o art. 274 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410020 = new cClassTribIBSCBS("410020", "Exclusão do valor de intermediação na base de cálculo no fornecimento de alimentação, observado o art. 274 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct410021 = new cClassTribIBSCBS("410021", "Contribuição de que trata o art. 149-A da Constituição Federal");
    public static final cClassTribIBSCBS cct410099 = new cClassTribIBSCBS("410099", "Operações não onerosas sem previsão de tributação, não especificadas anteriormente");
    public static final cClassTribIBSCBS cct510001 = new cClassTribIBSCBS("510001", "Operações, sujeitas a diferimento, com energia elétrica ou com direitos a ela relacionados, relativas à geração, comercialização, distribuição e transmissão, observado o art. 28 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct510002 = new cClassTribIBSCBS("510002", "Operações, sujeitas a diferimento, com insumos agropecuários e aquícolas destinados a produtor rural contribuinte, observado o art. 138 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550001 = new cClassTribIBSCBS("550001", "Exportações de bens materiais, observado o art. 82 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550002 = new cClassTribIBSCBS("550002", "Regime de Trânsito, observado o art. 84 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550003 = new cClassTribIBSCBS("550003", "Regimes de Depósito, observado o art. 85 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550004 = new cClassTribIBSCBS("550004", "Regimes de Depósito, observado o art. 87 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550005 = new cClassTribIBSCBS("550005", "Regimes de Depósito, observado o art. 87 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550006 = new cClassTribIBSCBS("550006", "Regimes de Permanência Temporária, observado o art. 88 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550007 = new cClassTribIBSCBS("550007", "Regimes de Aperfeiçoamento, observado o art. 90 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550008 = new cClassTribIBSCBS("550008", "Importação de bens para o Regime de Repetro-Temporário, de que tratam o inciso I do art. 93 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550009 = new cClassTribIBSCBS("550009", "GNL-Temporário, de que trata o inciso II do art. 93 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550010 = new cClassTribIBSCBS("550010", "Repetro-Permanente, de que trata o inciso III do art. 93 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550011 = new cClassTribIBSCBS("550011", "Repetro-Industrialização, de que trata o inciso IV do art. 93 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550012 = new cClassTribIBSCBS("550012", "Repetro-Nacional, de que trata o inciso V do art. 93 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550013 = new cClassTribIBSCBS("550013", "Repetro-Entreposto, de que trata o inciso VI do art. 93 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550014 = new cClassTribIBSCBS("550014", "Zona de Processamento de Exportação, observado os arts. 99, 100 e 102 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550015 = new cClassTribIBSCBS("550015", "Regime Tributário para Incentivo à Modernização e à Ampliação da Estrutura Portuária - Reporto, observado o art. 105 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550016 = new cClassTribIBSCBS("550016", "Regime Especial de Incentivos para o Desenvolvimento da Infraestrutura - Reidi, observado o art. 106 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550017 = new cClassTribIBSCBS("550017", "Regime Tributário para Incentivo à Atividade Econômica Naval – Renaval, observado o art. 107 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550018 = new cClassTribIBSCBS("550018", "Desoneração da aquisição de bens de capital, , observado o art. 109 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550019 = new cClassTribIBSCBS("550019", "Importação de bem material por indústria incentivada para utilização na Zona Franca de Manaus, observado o art. 443 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct550020 = new cClassTribIBSCBS("550020", "Áreas de livre comércio, observado o art. 461 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct620001 = new cClassTribIBSCBS("620001", "Tributação monofásica sobre combustíveis, observado o art. 172 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct620002 = new cClassTribIBSCBS("620002", "Tributação monofásica com responsabilidade pela retenção sobre combustíveis, observado o art. 178 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct620003 = new cClassTribIBSCBS("620003", "Tributação monofásica com tributos retidos por responsabilidade sobre combustíveis, observado o art. 178 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct620004 = new cClassTribIBSCBS("620004", "Tributação monofásica sobre mistura de EAC com gasolina A em percentual superior ou inferior ao obrigatório, observado o art. 179 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct620005 = new cClassTribIBSCBS("620005", "Tributação monofásica sobre combustíveis cobrada anteriormente, observador o art. 180 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct620006 = new cClassTribIBSCBS("620006", "Tributação monofásica sobre combustíveis cobrada anteriormente");
    public static final cClassTribIBSCBS cct800001 = new cClassTribIBSCBS("800001", "Fusão, cisão ou incorporação, observado o art. 55 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct800002 = new cClassTribIBSCBS("800002", "Transferência de crédito do associado, inclusive as cooperativas singulares, para cooperativa de que participa das operações antecedentes às operações em que fornece bens e serviços e os créditos presumidos, observado o art. 272 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct810001 = new cClassTribIBSCBS("810001", "Crédito presumido sobre o valor apurado nos fornecimentos a partir da Zona Franca de Manaus, observado o art. 450 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct820001 = new cClassTribIBSCBS("820001", "Documento com informações de fornecimento de serviços de planos de assinstência à saúde, mas com tributação realizada por outro meio, observado o art. 235 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct820002 = new cClassTribIBSCBS("820002", "Documento com informações de fornecimento de serviços de planos de assinstência funerária, mas com tributação realizada por outro meio, observado o art. 236 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct820003 = new cClassTribIBSCBS("820003", "Documento com informações de fornecimento de serviços de planos de assinstência à saúde de animais domésticos, mas com tributação realizada por outro meio, observado o art. 243 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct820004 = new cClassTribIBSCBS("820004", "Documento com informações de prestação de serviços de consursos de prognósticos, mas com tributação realizada por outro meio, observado o art. 248 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct820005 = new cClassTribIBSCBS("820005", "Documento com informações de alienação de bens imóveis, mas com tributação realizada por outro meio,, observado o art. 254 da Lei Complementar nº 214, de 2025.");
    public static final cClassTribIBSCBS cct830001 = new cClassTribIBSCBS("830001", "Documento com exclusão da BC da CBS e do IBS de energia elétrica fornecida pela distribuidora à UC");

    private final String value;
    private final String description;

    static {
        addToLookup(cctNenhum);
        addToLookup(cct000001);
        addToLookup(cct000002);
        addToLookup(cct000003);
        addToLookup(cct000004);
        addToLookup(cct010001);
        addToLookup(cct011001);
        addToLookup(cct011002);
        addToLookup(cct011003);
        addToLookup(cct011004);
        addToLookup(cct011005);
        addToLookup(cct200001);
        addToLookup(cct200002);
        addToLookup(cct200003);
        addToLookup(cct200004);
        addToLookup(cct200005);
        addToLookup(cct200006);
        addToLookup(cct200007);
        addToLookup(cct200008);
        addToLookup(cct200009);
        addToLookup(cct200010);
        addToLookup(cct200011);
        addToLookup(cct200012);
        addToLookup(cct200013);
        addToLookup(cct200014);
        addToLookup(cct200015);
        addToLookup(cct200016);
        addToLookup(cct200017);
        addToLookup(cct200018);
        addToLookup(cct200019);
        addToLookup(cct200020);
        addToLookup(cct200021);
        addToLookup(cct200022);
        addToLookup(cct200023);
        addToLookup(cct200024);
        addToLookup(cct200025);
        addToLookup(cct200026);
        addToLookup(cct200027);
        addToLookup(cct200028);
        addToLookup(cct200029);
        addToLookup(cct200030);
        addToLookup(cct200031);
        addToLookup(cct200032);
        addToLookup(cct200033);
        addToLookup(cct200034);
        addToLookup(cct200035);
        addToLookup(cct200036);
        addToLookup(cct200037);
        addToLookup(cct200038);
        addToLookup(cct200039);
        addToLookup(cct200040);
        addToLookup(cct200041);
        addToLookup(cct200042);
        addToLookup(cct200043);
        addToLookup(cct200044);
        addToLookup(cct200045);
        addToLookup(cct200046);
        addToLookup(cct200047);
        addToLookup(cct200048);
        addToLookup(cct200049);
        addToLookup(cct200051);
        addToLookup(cct200052);
        addToLookup(cct200450);
        addToLookup(cct210001);
        addToLookup(cct210002);
        addToLookup(cct210003);
        addToLookup(cct220001);
        addToLookup(cct220002);
        addToLookup(cct220003);
        addToLookup(cct221001);
        addToLookup(cct222001);
        addToLookup(cct400001);
        addToLookup(cct410001);
        addToLookup(cct410002);
        addToLookup(cct410003);
        addToLookup(cct410004);
        addToLookup(cct410005);
        addToLookup(cct410006);
        addToLookup(cct410007);
        addToLookup(cct410008);
        addToLookup(cct410009);
        addToLookup(cct410010);
        addToLookup(cct410011);
        addToLookup(cct410012);
        addToLookup(cct410013);
        addToLookup(cct410014);
        addToLookup(cct410015);
        addToLookup(cct410016);
        addToLookup(cct410017);
        addToLookup(cct410018);
        addToLookup(cct410019);
        addToLookup(cct410020);
        addToLookup(cct410021);
        addToLookup(cct410099);
        addToLookup(cct510001);
        addToLookup(cct510002);
        addToLookup(cct550001);
        addToLookup(cct550002);
        addToLookup(cct550003);
        addToLookup(cct550004);
        addToLookup(cct550005);
        addToLookup(cct550006);
        addToLookup(cct550007);
        addToLookup(cct550008);
        addToLookup(cct550009);
        addToLookup(cct550010);
        addToLookup(cct550011);
        addToLookup(cct550012);
        addToLookup(cct550013);
        addToLookup(cct550014);
        addToLookup(cct550015);
        addToLookup(cct550016);
        addToLookup(cct550017);
        addToLookup(cct550018);
        addToLookup(cct550019);
        addToLookup(cct550020);
        addToLookup(cct620001);
        addToLookup(cct620002);
        addToLookup(cct620003);
        addToLookup(cct620004);
        addToLookup(cct620005);
        addToLookup(cct620006);
        addToLookup(cct800001);
        addToLookup(cct800002);
        addToLookup(cct810001);
        addToLookup(cct820001);
        addToLookup(cct820002);
        addToLookup(cct820003);
        addToLookup(cct820004);
        addToLookup(cct820005);
        addToLookup(cct830001);
    }

    private cClassTribIBSCBS(String value, String description) {
        this.value = value;
        this.description = description;
    }

    private static void addToLookup(cClassTribIBSCBS cclassTribIBSCBS) {
        lookup.put(cclassTribIBSCBS.value, cclassTribIBSCBS);
    }

    public String getValue() {
        return value;
    }

    public String getDescription() {
        return description;
    }

    public static cClassTribIBSCBS fromValue(String value) {
        cClassTribIBSCBS cclassTribIBSCBS = lookup.get(value);
        if (cclassTribIBSCBS == null) {
            throw new IllegalArgumentException("cClassTribIBSCBS inválido: " + value);
        }
        return cclassTribIBSCBS;
    }

    @Override
    public String toString() {
        return value;
    }
}
