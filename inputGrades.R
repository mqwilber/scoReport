#' @title inputGrades
#'  
#' @description \code{inputGrades}
#'
#' @details
#' 
#' @param data_name A file name for the data to be loaded. Expects file
#'                  to be a csv and in the correct format. That is, the first 
#'                  \code{num_stud_vars} columns are non-grade items followed
#'                  by grade items.  The first tow rows are 1. maximum grades
#'                  for a grade column 2. Weights of the grades.
#' @param num_stud_vars The number of variables in the data file that are
#'                       non-grade variables. Should be before all grade
#'                       variables
#' @param normalize If \code{TRUE}, grades are normalized to be out of 100
#' @param reweight If \code{TRUE}, grades are weighted based on given weights
#'
#'
#' @export
#' 
#' @examples
#' 
#' @return An object of class \code{inputGrades} with elements
#' \describe{
#'   \item{\code{grades}}{Normalize and/or weighted grades in data.table}
#'   \item{\code{raw_max_grades}}{Vector of raw max grades for each assignment (unnormalized)}
#'   \item{\code{norm_max_grades}}{Vector of normalized and/or reweighted grades}
#'   \item{\code{weights}}{Vector of grade weights for each assignment}
#'   \item{\code{num_stud_vars}}{Number of columns in grades that are student variables (i.e. not grades)}
#' }
#'
#' @author Nate Emery, Mark Wilber 
#' @seealso 
#' @references 

# TODO: Check how to cleanly load packages in R package
library(data.table)

inputGrades = function(data_name, num_stud_vars, normalize=TRUE, 
                       reweight=TRUE){

    # Load in the data
    grades_raw = as.data.table(read.csv(data_name))

    gcols = (num_stud_vars + 1):ncol(grades_raw) # Grade columns
    max_grades = as.numeric(grades_raw[1, gcols, with=F])
    weights = as.numeric(grades_raw[2, gcols, with=F] / 
                            sum(grades_raw[2, gcols, with=F]))

    # TODO: Throw warning if weights don't equal 100 or 1

    student_cols = grades_raw[-c(1,2), 1:num_stud_vars, with=F]
    grade_cols = grades_raw[-c(1,2), gcols, with=F]
    norm_max_grades = max_grades

    if(normalize){  # Make all grades out of 100                                           
        grade_cols = as.data.table(t(t(grade_cols) / max_grades) * 100)
        norm_max_grades = rep(100, lenght(gcols)) #(max_grades / max_grades) * 100
    }

    if(reweight){ # Weight the grades based on given weights
        grade_cols = as.data.table(t(t(grade_cols) * weights))
        norm_max_grades = norm_max_grades * weights

    }

    formatted_grades = as.data.table(cbind(student_cols, grade_cols))

    out = list(grades=formatted_grades, 
                raw_max_grades=max_grades, 
                norm_max_grades=norm_max_grades,
                weights=weights,
                num_stud_vars=num_stud_vars)

    class(out) = c('inputGrades')

    return(out)
}
