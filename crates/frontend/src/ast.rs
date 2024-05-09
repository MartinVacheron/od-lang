use crate::parser::VarType;

#[derive(Debug, PartialEq, Clone)]
pub struct ASTNode {
    pub node: ASTNodeKind,
    pub line: u64,
}

impl ASTNode {
    pub fn new(node: ASTNodeKind, line: u64) -> Self {
        Self { node, line }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTNodeKind {
    Statement(StatementKind),
    Expression(ExpressionKind),
}

impl From<ExpressionKind> for ASTNodeKind {
    fn from(value: ExpressionKind) -> Self {
        ASTNodeKind::Expression(value)
    }
}

impl From<StatementKind> for ASTNodeKind {
    fn from(value: StatementKind) -> Self {
        ASTNodeKind::Statement(value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementKind {
    VarDeclaration {
        name: String,
        // Option is for declaration without value
        value: ExpressionKind,
        constant: bool,
        var_type: VarType,
    },
    VarCommaDeclaration {
        // Vector of VarDeclaration
        declarations: Vec<StatementKind>,
    },
    StructDeclaration {
        name: String,
        // Vector of member name + type + constant or not
        members: Vec<(String, VarType, bool)>,
        // Vec of StatementKind::FnDeclaration
        functions: Vec<StatementKind>,
    },
    FnDeclaration {
        name: String,
        args_and_type: Vec<(String, VarType)>,
        body: Vec<ASTNodeKind>,
        return_stmt: Option<ExpressionKind>,
        return_type: VarType,
    },
    TestDeclaration {
        name: String,
        body: Vec<ASTNodeKind>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionKind {
    IntLiteral {
        value: i64,
    },
    RealLiteral {
        value: f64,
    },
    Identifier {
        symbol: String,
    },
    ArrayLiteral {
        values: Vec<ExpressionKind>,
    },
    // Use to convey the information about a typed variable uninitialized
    // like: var a: Planet
    EmptyStructLiteral {
        name: String,
    },
    // Box needed to avoid recursion. We don't use ref + lifetime because not usefull.
    // I tried but working with boxes seems to be easier and more appropriate when
    // using unknown variable (left and right are build while running, if we wanted
    // to work with ref we would have to keep track of each ref of the elements created
    // in the parser.
    BinaryOp {
        left: Box<ExpressionKind>,
        right: Box<ExpressionKind>,
        operator: String,
    },
    // Variable assignment is an expression. It is like this:
    // x = x + 5    or    obj.member = x + 5
    // Chaining is allowed: x = y = z = 3
    // We have to let the left side as an Expression to be parsed correctly
    VarAssignment {
        assigne: Box<ExpressionKind>,
        value: Box<ExpressionKind>,
    },
    // Member call are recursive so in this case: planet.position.y, member
    // is gonna be planet.position and property y
    MemberCall {
        member: Box<ExpressionKind>,
        property: Box<ExpressionKind>,
    },
    FunctionCall {
        name: String,
        args: Vec<ExpressionKind>,
    },
    // Caller can ba a member can be planet.val[1]
    // Index can be any expr: val[5+6], val[pos.get_idx()]
    ArrayCall {
        name: String,
        index: ArrayIndexing,
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArrayIndexing {
    Single(Box<ExpressionKind>),
    Slice {
        start: Option<Box<ExpressionKind>>,
        end: Option<Box<ExpressionKind>>,
    },
}

impl ExpressionKind {
    // Get type of Expr
    pub(super) fn get_expr_type(&self) -> VarType {
        match self {
            // We allow implicit cast from int to real
            ExpressionKind::IntLiteral { .. } => VarType::Int,
            ExpressionKind::RealLiteral { .. } => VarType::Real,
            ExpressionKind::Identifier { symbol } => match symbol.as_str() {
                "true" | "false" => VarType::Bool,
                _ => VarType::Any
            },
            ExpressionKind::ArrayLiteral { .. } => VarType::Array(Box::new(VarType::Any)),
            _ => VarType::Any
        }
    }
}
